import { DataSyncController } from './ihp-datasync.js';
import { createControllerSubscriptionTransport, isTransportScopeChange } from './controller-subscription-transport.js';
import { createCountSubscriptionProtocol } from './count-subscription-protocol.js';
import { countResourceKeyForScope } from './resource-key.js';
import { createResourceRegistry } from './resource-registry.js';
import { countSubscriptionPolicy, type SubscriptionPublication } from './subscription-machine.js';
import { createSubscriptionResource, type SubscriptionResource } from './subscription-resource.js';
import { initialResourceSnapshot, type ResourceSnapshot } from './subscription-reducer.js';
import type { DynamicSQLQuery, ServerMessage, UUID } from './types.js';

/** Server-backed count resource used by the React hook and non-React callers. */
export class CountSubscription {
    query: DynamicSQLQuery;
    subscriptionId: UUID | null = null;
    onClose: () => void = () => {};
    onStoreClose: () => void = () => {};
    /** @internal Promotes a render-only weak registry entry on first retain. */
    onStoreRetain: () => void = () => {};
    subscribers = new Set<() => void>();

    private readonly serverQuery: DynamicSQLQuery;
    private readonly expectedTransportScopeKey: string;
    private readonly resource: SubscriptionResource<number>;
    private readonly subscriberEntries = new Map<number, () => void>();
    private readonly snapshotSubscriberEntries = new Map<number, () => void>();
    private nextSubscriberId = 0;
    private nextSnapshotSubscriberId = 0;

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
        this.resource = createSubscriptionResource<number>(
            createCountSubscriptionProtocol(this.serverQuery),
            {
                expectedScopeKey: this.expectedTransportScopeKey,
                currentScopeKey: () => DataSyncController.currentTransportScopeKey(),
                acquireTransport: () => {
                    const controller = DataSyncController.getInstance();
                    return createControllerSubscriptionTransport(
                        controller,
                        this.expectedTransportScopeKey,
                    );
                },
                policy: countSubscriptionPolicy,
                onState: state => {
                    this.subscriptionId = state.phase.tag === 'live'
                        ? state.phase.subscriptionId
                        : null;
                },
                publish: publication => this.publish(publication),
                evict: () => this.notifyClose(),
                reportError: (message, error) => console.error(message, error),
            },
        );
    }

    get count(): number | null {
        return this.resource.getSnapshot().data;
    }

    set count(count: number | null) {
        this.resource.dispatchCompatibilityValue(count);
    }

    get connectError(): Error | null {
        return this.resource.getSnapshot().error;
    }

    getSnapshot(): ResourceSnapshot<number> {
        return this.resource.getSnapshot();
    }

    getServerSnapshot(): ResourceSnapshot<number> {
        return initialResourceSnapshot();
    }

    getCount(): number | null {
        return this.resource.getSnapshot().data;
    }

    subscribe(callback: () => void): () => void {
        this.retainInStore();
        const id = this.nextSubscriberId++;
        this.subscriberEntries.set(id, callback);
        this.subscribers.add(callback);
        this.updateDemand(false);

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
            this.updateDemand(false);
        };
    }

    /** Internal external-store subscription; observes lifecycle and error changes. */
    subscribeSnapshot(callback: () => void): () => void {
        this.retainInStore();
        const id = this.nextSnapshotSubscriberId++;
        this.snapshotSubscriberEntries.set(id, callback);
        this.updateDemand(false);

        let subscribed = true;
        return () => {
            if (!subscribed) {
                return;
            }
            subscribed = false;
            this.snapshotSubscriberEntries.delete(id);
            this.updateDemand(false);
        };
    }

    async close(): Promise<void> {
        this.subscriberEntries.clear();
        this.snapshotSubscriberEntries.clear();
        this.subscribers.clear();
        await this.resource.close();
    }

    private onMessage(message: ServerMessage): void {
        this.resource.receiveMessage(message);
    }

    private onDataSyncClosed(event: unknown = null): void {
        this.resource.transportClosed(isTransportScopeChange(event));
    }

    private onDataSyncReconnect(): void {
        void this.resource.transportReconnected();
    }

    private updateDemand(imperative: boolean): void {
        const trackedCallbacks = new Set(this.subscriberEntries.values());
        let externalSubscribers = 0;
        for (const subscriber of this.subscribers) {
            if (!trackedCallbacks.has(subscriber)) {
                externalSubscribers++;
            }
        }
        this.resource.updateDemand(
            this.subscriberEntries.size
                + this.snapshotSubscriberEntries.size
                + externalSubscribers,
            imperative,
        );
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

    private publish(publication: SubscriptionPublication): void {
        if (publication === 'snapshot' || publication === 'both') {
            this.notifySnapshotSubscribers();
        }
        if (publication === 'legacy' || publication === 'both') {
            this.notifySubscribers();
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
}

type CountSubscriptionRegistryInput = Readonly<{
    key: string;
    query: DynamicSQLQuery;
    scope: string;
}>;

const countSubscriptionRegistry = createResourceRegistry<
    CountSubscriptionRegistryInput,
    string,
    CountSubscription
>({
    key: input => input.key,
    create: (input, lifecycle) => {
        const subscription = new CountSubscription(input.query, input.scope);
        subscription.onStoreRetain = () => lifecycle.retain(subscription);
        subscription.onStoreClose = () => lifecycle.close(subscription);
        return subscription;
    },
});

export class CountSubscriptionStore {
    static get queryMap(): Map<string, CountSubscription> {
        return countSubscriptionRegistry.active;
    }

    static set queryMap(next: Map<string, CountSubscription>) {
        countSubscriptionRegistry.replaceActive(next);
    }

    static get(query: DynamicSQLQuery): CountSubscription {
        const expectedTransportScopeKey = DataSyncController.currentTransportScopeKey();
        const key = countResourceKeyForScope(expectedTransportScopeKey, query);
        return countSubscriptionRegistry.getOrCreate({
            key,
            query,
            scope: expectedTransportScopeKey,
        });
    }
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
