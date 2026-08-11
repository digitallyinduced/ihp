import { DataSyncController } from './ihp-datasync.js';
import { dataSubscriptionKey } from './data-subscription-store.js';
import { WeakValueMap } from './weak-value-map.js';
import {
    initialResourceSnapshot,
    reduceResourceSnapshot,
    type ResourceSnapshot,
    type ResourceSnapshotAction,
} from './subscription-reducer.js';
import type { DynamicSQLQuery, ServerMessage, UUID } from './types.js';

class ResettableMap<K, V> extends Map<K, V> {
    constructor(private readonly resetPendingEntries: () => void) {
        super();
    }

    override clear(): void {
        super.clear();
        this.resetPendingEntries();
    }
}

/** Server-backed count resource used by the React hook and non-React callers. */
export class CountSubscription {
    query: DynamicSQLQuery;
    subscriptionId: UUID | null = null;
    onClose: () => void = () => {};
    onStoreClose: () => void = () => {};
    /** @internal Promotes a render-only weak registry entry on first retain. */
    onStoreRetain: () => void = () => {};
    subscribers = new Set<() => void>();

    private controller: DataSyncController | null = null;
    private readonly serverQuery: DynamicSQLQuery;
    private readonly expectedTransportScopeKey: string;
    private snapshot: ResourceSnapshot<number> = initialResourceSnapshot();
    private readonly subscriberEntries = new Map<number, () => void>();
    private readonly snapshotSubscriberEntries = new Map<number, () => void>();
    private nextSubscriberId = 0;
    private nextSnapshotSubscriberId = 0;
    private generation = 0;
    private started = false;
    private listenersAttached = false;
    private disposalToken = 0;
    private startPromise: Promise<void> | null = null;

    constructor(
        query: DynamicSQLQuery,
        expectedTransportScopeKey: string = DataSyncController.currentTransportScopeKey(),
    ) {
        this.query = JSON.parse(JSON.stringify(query)) as DynamicSQLQuery;
        this.serverQuery = deepFreeze(JSON.parse(JSON.stringify(query)) as DynamicSQLQuery);
        this.expectedTransportScopeKey = expectedTransportScopeKey;
        this.subscribe = this.subscribe.bind(this);
        this.subscribeSnapshot = this.subscribeSnapshot.bind(this);
        this.getCount = this.getCount.bind(this);
        this.getSnapshot = this.getSnapshot.bind(this);
        this.getServerSnapshot = this.getServerSnapshot.bind(this);
        this.onMessage = this.onMessage.bind(this);
        this.onDataSyncClosed = this.onDataSyncClosed.bind(this);
        this.onDataSyncReconnect = this.onDataSyncReconnect.bind(this);
    }

    get count(): number | null {
        return this.snapshot.data;
    }

    set count(count: number | null) {
        if (count === null) {
            this.snapshot = initialResourceSnapshot();
            this.notifySubscribers();
            this.notifySnapshotSubscribers();
        } else {
            this.dispatch({ type: 'SNAPSHOT', data: count });
        }
    }

    get connectError(): Error | null {
        return this.snapshot.error;
    }

    getSnapshot(): ResourceSnapshot<number> {
        return this.snapshot;
    }

    getServerSnapshot(): ResourceSnapshot<number> {
        return initialResourceSnapshot();
    }

    getCount(): number | null {
        return this.snapshot.data;
    }

    subscribe(callback: () => void): () => void {
        this.retainInStore();
        const shouldStart = this.trackedSubscriberCount() === 0;
        const id = this.nextSubscriberId++;
        this.subscriberEntries.set(id, callback);
        this.subscribers.add(callback);
        this.disposalToken++;
        if (shouldStart) {
            void this.start(false).catch(() => {
                // The immutable snapshot carries the error to the consumer.
            });
        }

        let subscribed = true;
        return () => {
            if (!subscribed) {
                return;
            }
            subscribed = false;
            this.subscriberEntries.delete(id);
            if (!Array.from(this.subscriberEntries.values()).includes(callback)) {
                this.subscribers.delete(callback);
            }
            this.scheduleStop();
        };
    }

    /** Internal external-store subscription; observes lifecycle and error changes. */
    subscribeSnapshot(callback: () => void): () => void {
        this.retainInStore();
        const shouldStart = this.trackedSubscriberCount() === 0;
        const id = this.nextSnapshotSubscriberId++;
        this.snapshotSubscriberEntries.set(id, callback);
        this.disposalToken++;
        if (shouldStart) {
            void this.start(false).catch(() => {
                // The immutable snapshot carries the error to the consumer.
            });
        }

        let subscribed = true;
        return () => {
            if (!subscribed) {
                return;
            }
            subscribed = false;
            this.snapshotSubscriberEntries.delete(id);
            this.scheduleStop();
        };
    }

    async close(): Promise<void> {
        this.subscriberEntries.clear();
        this.snapshotSubscriberEntries.clear();
        this.subscribers.clear();
        this.disposalToken++;
        await this.stop();
    }

    private start(reconnect: boolean): Promise<void> {
        this.disposalToken++;
        if (this.startPromise !== null) {
            return this.startPromise;
        }
        if (this.started && this.subscriptionId !== null) {
            return Promise.resolve();
        }

        if (!this.hasExpectedTransportScope()) {
            this.closeForTransportScopeChange();
            return Promise.resolve();
        }

        const controller = DataSyncController.getInstance();
        if (!controller.isBoundToTransportScope(this.expectedTransportScopeKey)) {
            this.closeForTransportScopeChange();
            return Promise.resolve();
        }
        this.attachListeners(controller);
        this.started = true;
        const generation = ++this.generation;
        this.dispatch({ type: 'CONNECT', reconnect });
        const startPromise = this.createOnServer(generation, controller);
        this.startPromise = startPromise;
        void startPromise.finally(() => {
            if (this.startPromise === startPromise) {
                this.startPromise = null;
            }
        }).catch(() => {});
        return startPromise;
    }

    private async createOnServer(generation: number, controller: DataSyncController): Promise<void> {
        try {
            const response = await controller.sendMessage({
                tag: 'CreateCountSubscription',
                query: this.serverQuery,
            });
            const subscriptionId = response.subscriptionId as UUID;
            if (!this.started
                || generation !== this.generation
                || this.controller !== controller
                || !this.hasExpectedTransportScope()
                || !controller.isBoundToTransportScope(this.expectedTransportScopeKey)) {
                if (!this.hasExpectedTransportScope()) {
                    this.closeForTransportScopeChange();
                }
                await this.deleteOnServer(controller, subscriptionId);
                return;
            }

            this.subscriptionId = subscriptionId;
            this.dispatch({ type: 'SNAPSHOT', data: response.count as number });
        } catch (unknownError) {
            if (!this.started || generation !== this.generation || this.controller !== controller) {
                return;
            }
            const error = unknownError instanceof Error ? unknownError : new Error(String(unknownError));
            this.dispatch({ type: 'FAIL', error });
            throw error;
        }
    }

    private attachListeners(controller: DataSyncController): void {
        if (this.listenersAttached && this.controller === controller) {
            return;
        }
        this.detachListeners();
        this.controller = controller;
        this.listenersAttached = true;
        controller.addEventListener('message', this.onMessage);
        controller.addEventListener('close', this.onDataSyncClosed);
        controller.addEventListener('reconnect', this.onDataSyncReconnect);
    }

    private detachListeners(): void {
        const controller = this.controller;
        if (controller === null) {
            this.listenersAttached = false;
            return;
        }
        if (this.listenersAttached) {
            this.listenersAttached = false;
            controller.removeEventListener('message', this.onMessage);
            controller.removeEventListener('close', this.onDataSyncClosed);
            controller.removeEventListener('reconnect', this.onDataSyncReconnect);
        }
        this.controller = null;
    }

    private onMessage(message: ServerMessage): void {
        if (!this.hasExpectedTransportScope()
            || this.controller?.isBoundToTransportScope(this.expectedTransportScopeKey) !== true) {
            this.closeForTransportScopeChange();
            return;
        }
        if (message.tag === 'DidChangeCount' && message.subscriptionId === this.subscriptionId) {
            this.dispatch({ type: 'SNAPSHOT', data: message.count as number });
        }
    }

    private onDataSyncClosed(event: unknown = null): void {
        if (!this.started) {
            return;
        }
        if (isTransportScopeChange(event)) {
            this.closeForTransportScopeChange();
            return;
        }
        this.generation++;
        this.subscriptionId = null;
        this.startPromise = null;
        this.dispatch({ type: 'DISCONNECT', clearData: true });
    }

    private onDataSyncReconnect(): void {
        if (!this.started || this.startPromise !== null || this.subscriptionId !== null) {
            return;
        }
        void this.start(true).catch(() => {
            // The immutable snapshot carries the error to the consumer.
        });
    }

    private scheduleStop(): void {
        const token = ++this.disposalToken;
        queueMicrotask(() => {
            if (token === this.disposalToken
                && this.subscribers.size === 0
                && this.snapshotSubscriberEntries.size === 0) {
                void this.stop();
            }
        });
    }

    private async stop(): Promise<void> {
        if (!this.started && this.snapshot.status === 'closed') {
            return;
        }
        this.started = false;
        this.generation++;
        this.startPromise = null;
        const subscriptionId = this.subscriptionId;
        const controller = this.controller;
        this.subscriptionId = null;
        this.detachListeners();
        this.dispatch({ type: 'CLOSE' });
        this.notifyClose();
        if (subscriptionId !== null && controller !== null) {
            await this.deleteOnServer(controller, subscriptionId);
        }
    }

    private async deleteOnServer(controller: DataSyncController, subscriptionId: UUID): Promise<void> {
        try {
            await controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId });
        } catch (error) {
            if (!controller.retired && controller.connection !== null) {
                console.error('Failed to delete a stale CountSubscription:', error);
            }
        }
    }

    private closeForTransportScopeChange(): void {
        this.started = false;
        this.generation++;
        this.subscriptionId = null;
        this.startPromise = null;
        this.detachListeners();
        this.snapshot = initialResourceSnapshot();
        this.dispatch({ type: 'CLOSE' });
        this.notifyClose();
    }

    private dispatch(action: ResourceSnapshotAction<number>): void {
        const nextSnapshot = reduceResourceSnapshot(this.snapshot, action);
        if (nextSnapshot === this.snapshot) {
            return;
        }
        this.snapshot = nextSnapshot;
        this.notifySnapshotSubscribers();
        if (action.type === 'SNAPSHOT' || action.type === 'DISCONNECT') {
            this.notifySubscribers();
        }
    }

    private trackedSubscriberCount(): number {
        return this.subscriberEntries.size + this.snapshotSubscriberEntries.size;
    }

    private retainInStore(): void {
        try {
            this.onStoreRetain();
        } catch (error) {
            console.error('CountSubscription store-retain listener failed:', error);
        }
    }

    private notifySnapshotSubscribers(): void {
        for (const subscriber of Array.from(this.snapshotSubscriberEntries.values())) {
            try {
                subscriber();
            } catch (error) {
                console.error('CountSubscription snapshot subscriber failed:', error);
            }
        }
    }

    private notifySubscribers(): void {
        for (const subscriber of Array.from(this.subscribers)) {
            try {
                subscriber();
            } catch (error) {
                console.error('CountSubscription subscriber failed:', error);
            }
        }
    }

    private notifyClose(): void {
        try {
            this.onStoreClose();
        } catch (error) {
            console.error('CountSubscription store-close listener failed:', error);
        }
        try {
            this.onClose();
        } catch (error) {
            console.error('CountSubscription close listener failed:', error);
        }
    }

    private hasExpectedTransportScope(): boolean {
        return this.expectedTransportScopeKey === DataSyncController.currentTransportScopeKey();
    }
}

export class CountSubscriptionStore {
    private static readonly pendingQueryMap = new WeakValueMap<string, CountSubscription>();
    static readonly queryMap = new ResettableMap<string, CountSubscription>(() => {
        CountSubscriptionStore.pendingQueryMap.clear();
    });

    static get(query: DynamicSQLQuery): CountSubscription {
        const key = `count:${dataSubscriptionKey(query)}`;
        const existing = this.queryMap.get(key);
        if (existing !== undefined) {
            return existing;
        }
        const pending = this.pendingQueryMap.get(key);
        if (pending !== undefined) {
            return pending;
        }

        const expectedTransportScopeKey = DataSyncController.currentTransportScopeKey();
        const subscription = new CountSubscription(query, expectedTransportScopeKey);
        subscription.onStoreRetain = () => {
            this.queryMap.set(key, subscription);
        };
        subscription.onStoreClose = () => {
            if (this.queryMap.get(key) === subscription) {
                this.queryMap.delete(key);
            }
            this.pendingQueryMap.delete(key, subscription);
        };
        this.pendingQueryMap.set(key, subscription);
        return subscription;
    }
}

function isTransportScopeChange(event: unknown): boolean {
    return typeof event === 'object'
        && event !== null
        && 'type' in event
        && (event as { type?: unknown }).type === 'transport-scope-changed';
}

function deepFreeze<T>(value: T): T {
    if (typeof value !== 'object' || value === null || Object.isFrozen(value)) {
        return value;
    }
    for (const nestedValue of Object.values(value as Record<string, unknown>)) {
        deepFreeze(nestedValue);
    }
    return Object.freeze(value);
}
