import {
    initialResourceSnapshot,
    reduceResourceSnapshot,
    type ResourceSnapshot,
    type ResourceSnapshotAction,
} from './subscription-reducer.js';
import type {
    DynamicSQLQuery,
    DataRecord,
    UUID,
    TableName,
    IHPRecord,
    NewRecord,
    DataSyncEventType,
    DataSyncEventMap,
    PendingRequest,
    ServerMessage,
    DataSubscriptionOptions,
    CrudOptions,
} from './types.js';
import { APPEND_NEW_RECORD, PREPEND_NEW_RECORD, NewRecordBehaviour } from './types.js';

type EventListeners = {
    [K in DataSyncEventType]: DataSyncEventMap[K][];
};

type OutboxMessage = {
    requestId: number;
    payload: string;
};

type PendingCreate = {
    promise: Promise<void>;
    resolve: () => void;
    reject: (reason: Error) => void;
};

type TransportScope = Readonly<{
    backendHost: string | null;
    jwt: string | null;
    origin: string;
    key: string;
}>;

const transportScopeChanged = Object.freeze({ type: 'transport-scope-changed' });

function isTransportScopeChange(event: unknown): boolean {
    return event === transportScopeChanged;
}

class DataSyncController {
    static instance: DataSyncController | null = null;
    static ihpBackendHost: string | null = null;
    private static readonly instanceListeners = new Set<(controller: DataSyncController | null) => void>();
    private static retiringCurrentTransport = false;
    private static authSessionGeneration = 0;

    static getInstance(): DataSyncController {
        if (DataSyncController.retiringCurrentTransport) {
            throw new Error('Cannot acquire a DataSync controller while the current transport is being retired');
        }
        const scope = DataSyncController.currentTransportScope();
        const current = DataSyncController.instance;
        if (current === null || current.transportScope.key !== scope.key || current.retired) {
            const next = new DataSyncController(scope);
            DataSyncController.instance = next;
            if (current !== null) {
                current.retire();
            }
            DataSyncController.notifyInstanceListeners(next);
        }

        return DataSyncController.instance!;
    }

    /** Returns the controller only if it belongs to the current auth/backend scope. */
    static peekInstance(): DataSyncController | null {
        const current = DataSyncController.instance;
        if (current === null || current.retired) {
            return null;
        }
        return current.transportScope.key === DataSyncController.currentTransportScope().key
            ? current
            : null;
    }

    static addInstanceListener(listener: (controller: DataSyncController | null) => void): () => void {
        DataSyncController.instanceListeners.add(listener);
        return () => DataSyncController.instanceListeners.delete(listener);
    }

    static currentTransportScopeKey(): string {
        return DataSyncController.currentTransportScope().key;
    }

    /**
     * Invalidates all DataSync resources belonging to the previous auth
     * session and closes its transport. Call this after replacing or clearing
     * a cookie-authenticated session, before rendering the next user's tree.
     *
     * Cookie contents (especially HttpOnly cookies) are intentionally opaque
     * to JavaScript, so this explicit boundary is required even when the
     * backend host and JWT have not changed.
     */
    static authSessionDidChange(): void {
        DataSyncController.authSessionGeneration++;
        DataSyncController.retireCurrentTransport();
    }

    /**
     * Retires the current cookie/JWT transport without creating a replacement.
     * Intended for explicit auth/config transitions in a commit or event phase.
     */
    static retireCurrentTransport(): void {
        if (DataSyncController.retiringCurrentTransport || DataSyncController.instance === null) {
            return;
        }
        DataSyncController.retiringCurrentTransport = true;
        try {
            const current = DataSyncController.instance;
            DataSyncController.instance = null;
            current?.retire();
            // Notify while acquisition is still blocked. A re-entrant listener
            // cannot install a replacement that would survive an auth reset.
            DataSyncController.notifyInstanceListeners(null);
            DataSyncController.instance = null;
        } finally {
            DataSyncController.retiringCurrentTransport = false;
        }
    }

    private static notifyInstanceListeners(controller: DataSyncController | null): void {
        for (const listener of Array.from(DataSyncController.instanceListeners)) {
            try {
                listener(controller);
            } catch (error) {
                console.error('DataSync controller instance listener failed:', error);
            }
        }
    }

    private static currentTransportScope(): TransportScope {
        let jwt: string | null = null;
        try {
            if (typeof localStorage !== 'undefined') {
                jwt = localStorage.getItem('ihp_jwt');
            }
        } catch (_error) {
            // localStorage can be unavailable during SSR or in privacy modes.
        }

        let origin = 'same-origin';
        try {
            if (typeof location !== 'undefined' && typeof location.origin === 'string') {
                origin = location.origin;
            }
        } catch (_error) {
            // Keep the stable SSR fallback.
        }

        const backendHost = DataSyncController.ihpBackendHost;
        return {
            backendHost,
            jwt,
            origin,
            key: JSON.stringify([
                backendHost ?? origin,
                jwt,
                DataSyncController.authSessionGeneration,
            ]),
        };
    }

    static getWSUrl(): string {
        return DataSyncController.webSocketUrl(DataSyncController.currentTransportScope());
    }

    private static webSocketUrl(scope: TransportScope): string {
        if (scope.backendHost) {
            const host = scope.backendHost
                .replace('https://', 'wss://')
                .replace('http://', 'ws://');
            return host + '/DataSyncController' + (scope.jwt !== null ? '?access_token=' + encodeURIComponent(scope.jwt) : '');
        }

        const socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
        const socketHost = socketProtocol + "://" + document.location.hostname + ":" + document.location.port + '/DataSyncController';
        return socketHost;
    }

    pendingRequests: PendingRequest[];
    connection: WebSocket | null;
    requestIdCounter: number;
    receivedFirstResponse: boolean;
    eventListeners: EventListeners;
    outbox: OutboxMessage[];
    reconnectTimeout: ReturnType<typeof setTimeout> | null;
    dataSubscriptions: DataSubscription[];
    pendingCreates: Map<UUID, PendingCreate>;
    /** @deprecated Optimistic updates are disabled; retained as an empty compatibility field. */
    optimisticCreatedPendingRecordIds: UUID[];
    /** @deprecated Alias for pendingCreates. */
    pendingOptimisticCreates: Map<UUID, PendingCreate>;
    /** @deprecated Optimistic shape inference is disabled. */
    optimisticCreatedNeedsCreatedAtField: Set<string>;
    messageTimeout: number;
    connectionAttemptTimeout: number;
    connectionRetryLimit: number;
    connectionRetryMaxDelayExponent: number;
    pendingConnection: Promise<WebSocket> | null;
    private readonly transportScope: TransportScope;
    retired: boolean;
    private readonly connectionAttemptAborters = new Set<(reason: Error) => void>();

    constructor(scope: TransportScope = DataSyncController.currentTransportScope()) {
        this.transportScope = scope;
        this.pendingRequests = [];
        this.connection = null;
        this.requestIdCounter = 0;
        this.receivedFirstResponse = false;
        this.eventListeners = {
            message: [],
            close: [],
            reconnect: [],
            open: []
        };

        this.outbox = [];
        this.reconnectTimeout = null;
        this.dataSubscriptions = [];
        this.pendingCreates = new Map();
        this.optimisticCreatedPendingRecordIds = [];
        this.pendingOptimisticCreates = this.pendingCreates;
        this.optimisticCreatedNeedsCreatedAtField = new Set();
        this.messageTimeout = 5000;
        this.connectionAttemptTimeout = 5000;
        this.connectionRetryLimit = 32;
        this.connectionRetryMaxDelayExponent = 6;
        this.pendingConnection = null;
        this.retired = false;
    }

    isBoundToTransportScope(scopeKey: string): boolean {
        return !this.retired && this.transportScope.key === scopeKey;
    }

    hasCurrentTransportScope(): boolean {
        return this.isBoundToTransportScope(DataSyncController.currentTransportScope().key);
    }

    private rejectScopeMismatch(): Error {
        const error = new Error('DataSync controller transport scope no longer matches the current authentication/backend scope');
        if (DataSyncController.instance === this) {
            DataSyncController.retireCurrentTransport();
        } else {
            this.retire();
        }
        return error;
    }

    async startConnection(): Promise<WebSocket> {
        if (!this.hasCurrentTransportScope()) {
            throw this.rejectScopeMismatch();
        }
        if (this.retired) {
            throw new Error('DataSync controller transport scope is no longer active');
        }
        if (this.connection) {
            return this.connection;
        }

        if (this.pendingConnection) {
            return await this.pendingConnection;
        }

        let pendingConnection!: Promise<WebSocket>;
        pendingConnection = (async () => {
            const connect = (): Promise<{ socket: WebSocket; event: Event }> => new Promise((resolve, reject) => {
                const socket = new WebSocket(DataSyncController.webSocketUrl(this.transportScope));
                let settled = false;

                const clearHandlers = () => {
                    socket.onopen = null;
                    socket.onerror = null;
                    socket.onclose = null;
                };
                const closeSocket = () => {
                    try {
                        socket.close();
                    } catch (_error) {
                        // Test doubles and partially constructed sockets may not be closable.
                    }
                };
                const fail = (error: unknown) => {
                    if (settled) {
                        return;
                    }
                    settled = true;
                    clearTimeout(attemptTimeout);
                    this.connectionAttemptAborters.delete(abort);
                    clearHandlers();
                    closeSocket();
                    reject(error);
                };
                const abort = (reason: Error) => fail(reason);
                const attemptTimeout = setTimeout(() => {
                    fail(new Error(`DataSync WebSocket connection attempt timed out after ${this.connectionAttemptTimeout}ms`));
                }, this.connectionAttemptTimeout);
                this.connectionAttemptAborters.add(abort);

                socket.onopen = (event) => {
                    if (settled || this.retired || !this.hasCurrentTransportScope()) {
                        if (!settled && !this.retired) {
                            fail(this.rejectScopeMismatch());
                            return;
                        }
                        closeSocket();
                        return;
                    }
                    settled = true;
                    clearTimeout(attemptTimeout);
                    this.connectionAttemptAborters.delete(abort);
                    this.connection = socket;
                    socket.onclose = (closeEvent) => this.onClose(closeEvent, socket);
                    socket.onmessage = this.onMessage.bind(this);
                    resolve({ socket, event });
                };

                socket.onerror = (event) => fail(event);
                socket.onclose = () => fail(new Error('DataSync WebSocket closed while the connection was opening'));
            });
            const wait = (timeout: number): Promise<void> => new Promise((resolve) => setTimeout(resolve, timeout));
            try {
                for (let i = 0; i < this.connectionRetryLimit; i++) {
                    if (this.retired) {
                        throw new Error('DataSync controller transport scope is no longer active');
                    }
                    try {
                        const { socket, event } = await connect();
                        if (this.connection !== socket) {
                            throw new Error('DataSync WebSocket closed while the connection was opening');
                        }
                        this.flushOutbox(socket);
                        if (this.retired
                            || !this.hasCurrentTransportScope()
                            || this.connection !== socket) {
                            throw new Error('DataSync WebSocket became obsolete before it could be used');
                        }

                        for (const listener of this.eventListeners.open.slice()) {
                            try {
                                listener(event);
                            } catch (error) {
                                console.error('DataSync open listener failed:', error);
                            }
                        }

                        return socket;
                    } catch (error) {
                        if (this.retired) {
                            throw error;
                        }
                        if (i === this.connectionRetryLimit - 1) {
                            throw error;
                        }
                        const time = Math.pow(2, Math.min(i, this.connectionRetryMaxDelayExponent));
                        console.log('Retrying in ', time, 'secs');
                        await wait(time * 1000);
                    }
                }

                throw new Error('Unable to connect to the DataSync Websocket');
            } finally {
                if (this.pendingConnection === pendingConnection) {
                    this.pendingConnection = null;
                }
            }
        })();
        this.pendingConnection = pendingConnection;
        return await pendingConnection;
    }

    onMessage(event: MessageEvent): void {
        if (!this.hasCurrentTransportScope()) {
            this.rejectScopeMismatch();
            return;
        }
        if (this.retired) {
            return;
        }
        const payload: ServerMessage = JSON.parse(event.data as string);
        const requestId = payload.requestId;
        const request = this.pendingRequests.find(request => request.requestId === requestId);

        if (request) {
            this.removePendingRequest(request);
            const { resolve, reject } = request;

            if (payload.tag === 'DataSyncError') {
                reject(new Error(payload.errorMessage as string));
            } else {
                resolve(payload);
            }
        } else {
            if (payload.tag === 'FailedToDecodeMessageError') {
                throw new Error(payload.errorMessage as string);
            }
        }

        this.receivedFirstResponse = true;
        for (const callback of this.eventListeners.message.slice()) {
            try {
                callback(payload);
            } catch (error) {
                console.error('DataSync message listener failed:', error);
            }
        }
    }

    onClose(_event: CloseEvent | null, closedSocket: WebSocket | null = null): void {
        if (this.retired) {
            return;
        }
        if (closedSocket !== null && this.connection !== closedSocket) {
            return;
        }

        this.connection = null;
        this.rejectSentPendingRequests(new Error('DataSync WebSocket closed before the server responded'));

        for (const listener of this.eventListeners.close.slice()) {
            try {
                listener(_event);
            } catch (error) {
                console.error('DataSync close listener failed:', error);
            }
        }

        if (!this.retired) {
            this.retryToReconnect();
        }
    }

    async sendMessage(payload: Record<string, unknown>): Promise<ServerMessage> {
        if (!this.hasCurrentTransportScope()) {
            throw this.rejectScopeMismatch();
        }
        if (this.retired) {
            throw new Error('DataSync controller transport scope is no longer active');
        }
        return new Promise((resolve, reject) => {
            payload.requestId = this.requestIdCounter++;
            const requestId = payload.requestId as number;
            const pendingRequest: PendingRequest = { requestId, resolve, reject, timeout: null, sent: false };
            const outboxMessage = { requestId, payload: JSON.stringify(payload) };
            this.pendingRequests.push(pendingRequest);

            if (this.connection === null) {
                this.outbox.push(outboxMessage);

                if (this.reconnectTimeout === null) {
                    void this.startConnection().catch((error: unknown) => {
                        this.rejectPendingRequest(requestId, new Error(`Unable to connect to the DataSync WebSocket: ${String(error)}`));
                    });
                }
            } else {
                this.sendPendingRequest(this.connection, outboxMessage);
            }
        });
    }

    private flushOutbox(connection: WebSocket): void {
        const queuedMessages = this.outbox;
        this.outbox = [];
        for (const message of queuedMessages) {
            this.sendPendingRequest(connection, message);
        }
    }

    private sendPendingRequest(connection: WebSocket, message: OutboxMessage): void {
        const request = this.pendingRequests.find(({ requestId }) => requestId === message.requestId);
        if (!request) {
            return;
        }
        if (this.retired) {
            this.rejectPendingRequest(
                request.requestId,
                new Error('DataSync controller transport scope is no longer active'),
            );
            return;
        }
        if (!this.hasCurrentTransportScope()) {
            this.rejectPendingRequest(request.requestId, this.rejectScopeMismatch());
            return;
        }
        if (connection !== this.connection) {
            this.rejectPendingRequest(
                request.requestId,
                new Error('DataSync WebSocket closed before the request could be sent'),
            );
            return;
        }

        try {
            connection.send(message.payload);
            request.sent = true;
            request.timeout = setTimeout(() => this.onPendingRequestTimeout(request.requestId), this.messageTimeout);
        } catch (error) {
            this.rejectPendingRequest(request.requestId, error);
        }
    }

    private removePendingRequest(request: PendingRequest): void {
        if (request.timeout !== null) {
            clearTimeout(request.timeout);
        }
        const index = this.pendingRequests.indexOf(request);
        if (index !== -1) {
            this.pendingRequests.splice(index, 1);
        }
        const outboxIndex = this.outbox.findIndex(({ requestId }) => requestId === request.requestId);
        if (outboxIndex !== -1) {
            this.outbox.splice(outboxIndex, 1);
        }
    }

    private rejectPendingRequest(requestId: number, reason: unknown): void {
        const request = this.pendingRequests.find(request => request.requestId === requestId);
        if (!request) {
            return;
        }
        this.removePendingRequest(request);
        request.reject(reason);
    }

    private rejectSentPendingRequests(reason: Error): void {
        for (const request of this.pendingRequests.slice()) {
            if (request.sent) {
                this.rejectPendingRequest(request.requestId, reason);
            }
        }
    }

    private rejectAllPendingRequests(reason: Error): void {
        for (const request of this.pendingRequests.slice()) {
            this.rejectPendingRequest(request.requestId, reason);
        }
        this.outbox = [];
    }

    private retire(): void {
        if (this.retired) {
            return;
        }
        this.retired = true;
        if (this.reconnectTimeout !== null) {
            clearTimeout(this.reconnectTimeout);
            this.reconnectTimeout = null;
        }

        const reason = new Error('DataSync transport scope changed');
        for (const abort of Array.from(this.connectionAttemptAborters)) {
            abort(reason);
        }
        this.connectionAttemptAborters.clear();
        this.rejectAllPendingRequests(reason);
        for (const pendingCreate of this.pendingCreates.values()) {
            pendingCreate.reject(reason);
        }
        this.pendingCreates.clear();

        const connection = this.connection;
        this.connection = null;
        if (connection !== null) {
            connection.onmessage = null;
            connection.onclose = null;
            try {
                connection.close();
            } catch (_error) {
                // Test doubles and already closed sockets need no further work.
            }
        }

        for (const listener of this.eventListeners.close.slice()) {
            try {
                listener(transportScopeChanged);
            } catch (error) {
                console.error('DataSync close listener failed during transport rotation:', error);
            }
        }
    }

    addEventListener<E extends DataSyncEventType>(event: E, callback: DataSyncEventMap[E]): void {
        (this.eventListeners[event] as DataSyncEventMap[E][]).push(callback);
    }

    removeEventListener<E extends DataSyncEventType>(event: E, callback: DataSyncEventMap[E]): void {
        const listeners = this.eventListeners[event] as DataSyncEventMap[E][];
        const index = listeners.indexOf(callback);
        if (index > -1) {
            listeners.splice(index, 1);
        }
    }

    retryToReconnect(): void {
        if (this.connection || this.retired) {
            return;
        }

        if (this.reconnectTimeout) {
            clearTimeout(this.reconnectTimeout);
        }
        this.reconnectTimeout = setTimeout(async () => {
            this.reconnectTimeout = null;
            if (this.retired) {
                return;
            }
            try {
                console.log('Trying to reconnect DataSync ...');
                await this.startConnection();

                for (const listener of this.eventListeners.reconnect.slice()) {
                    try {
                        listener();
                    } catch (error) {
                        console.error('DataSync reconnect listener failed:', error);
                    }
                }
            } catch (error) {
                console.error('DataSync reconnection failed:', error);
                this.retryToReconnect();
            }
        }, 1000);
    }

    /** @deprecated Server snapshots replaced optimistic shape inference. */
    learnOptimisticShapeFromResult(_table: string, _result: DataRecord[]): void {}

    onPendingRequestTimeout(requestId: number): void {
        const request = this.pendingRequests.find(request => request.requestId === requestId);
        if (!request) {
            return;
        }

        this.rejectPendingRequest(requestId, new Error(`DataSync request ${requestId} timed out after ${this.messageTimeout}ms`));
        if (this.connection) {
            console.log('Pending request timed out, closing WebSocket');
            this.connection.close();
        }
    }
}

class DataSubscription {
    query: DynamicSQLQuery;
    cache: Map<string, DataRecord[]> | null;
    /** @deprecated Server snapshots make client-side insertion placement a no-op. */
    newRecordBehaviour: number;
    subscriptionId: UUID | null = null;
    createOnServerPromise: Promise<DataRecord[]>;
    /** @deprecated Prefer createOnServerPromise; retained for source compatibility. */
    resolveCreateOnServer!: (value: DataRecord[]) => void;
    /** @deprecated Prefer createOnServerPromise; retained for source compatibility. */
    rejectCreateOnServer!: (reason: Error) => void;
    onClose: () => void = () => {};
    onStoreClose: () => void = () => {};
    /** @internal Promotes a render-only weak registry entry on first retain. */
    onStoreRetain: () => void = () => {};
    subscribers: Array<(records: DataRecord[] | null) => void> = [];
    isClosed = false;
    isConnected = false;
    connectError: Error | null = null;
    /** @deprecated Optimistic mutation is disabled; retained empty for compatibility. */
    optimisticCreatedPendingRecordIds: UUID[] = [];
    /** @deprecated Optimistic mutation is disabled; retained empty for compatibility. */
    optimisticUpdatedPendingRecordIds: Set<UUID> = new Set();

    private controller: DataSyncController | null = null;
    private readonly serverQuery: DynamicSQLQuery;
    private readonly expectedTransportScopeKey: string;
    private readonly scopedCache: Map<string, DataRecord[]> | null;
    private readonly cacheKey: string;
    private snapshot: ResourceSnapshot<DataRecord[]>;
    private readonly subscriberEntries = new Map<number, (records: DataRecord[] | null) => void>();
    private readonly snapshotSubscriberEntries = new Map<number, () => void>();
    private nextSubscriberId = 0;
    private nextSnapshotSubscriberId = 0;
    private generation = 0;
    private lastRevision = -1;
    private started = false;
    private listenersAttached = false;
    private closeNotificationSent = false;
    private disposalToken = 0;
    private startPromise: Promise<void> | null = null;
    private refreshPromise: Promise<void> | null = null;
    private refreshRequested = false;
    private supportsAuthoritativeSnapshots = false;
    private initialCreateSettled = false;
    private imperativelyOwned = false;
    private resolveInitialCreate!: (records: DataRecord[]) => void;
    private rejectInitialCreate!: (error: Error) => void;

    constructor(
        query: DynamicSQLQuery,
        options: DataSubscriptionOptions | null = null,
        cache: Map<string, DataRecord[]> | null = null,
        cacheKey: string = JSON.stringify(query),
        expectedTransportScopeKey: string = DataSyncController.currentTransportScopeKey(),
    ) {
        if (typeof query !== 'object' || query === null || !('table' in query)) {
            throw new Error("Query passed to `new DataSubscription(..)` doesn't look like a query object. If you're using the `query()` functions to construct the object, pass the `.query` property, like this: `new DataSubscription(query('my_table').orderBy('createdAt').query)`");
        }

        // QueryBuilder is mutable. A live subscription must never change when a
        // component reuses and mutates the builder after this point.
        this.query = cloneQuery(query);
        this.serverQuery = deepFreeze(cloneQuery(query));
        this.expectedTransportScopeKey = expectedTransportScopeKey;
        this.scopedCache = cache;
        this.cache = cache;
        this.cacheKey = cacheKey;
        const cachedRecords = cache?.get(cacheKey);
        this.snapshot = initialResourceSnapshot(
            cachedRecords?.map(record => ({ ...record })) as DataRecord[] | undefined,
        );
        this.createOnServerPromise = new Promise((resolve, reject) => {
            this.resolveInitialCreate = resolve;
            this.rejectInitialCreate = reject;
        });
        this.resolveCreateOnServer = records => this.resolveInitialCreateOnce(records);
        this.rejectCreateOnServer = error => this.rejectInitialCreateOnce(error);
        void this.createOnServerPromise.catch(() => {});
        this.newRecordBehaviour = options?.newRecordBehaviour ?? this.detectNewRecordBehaviour();

        this.subscribe = this.subscribe.bind(this);
        this.subscribeSnapshot = this.subscribeSnapshot.bind(this);
        this.getRecords = this.getRecords.bind(this);
        this.getSnapshot = this.getSnapshot.bind(this);
        this.getServerSnapshot = this.getServerSnapshot.bind(this);
        this.onMessage = this.onMessage.bind(this);
        this.onDataSyncClosed = this.onDataSyncClosed.bind(this);
        this.onDataSyncReconnect = this.onDataSyncReconnect.bind(this);
    }

    get records(): DataRecord[] | null {
        return this.snapshot.data;
    }

    /** @deprecated Query snapshots are server-owned; assigning is retained for compatibility only. */
    set records(records: DataRecord[] | null) {
        if (records === null) {
            this.snapshot = initialResourceSnapshot();
            this.notifySubscribers();
            this.notifySnapshotSubscribers();
        } else {
            this.applyServerSnapshot(records, this.lastRevision + 1);
        }
    }

    getSnapshot(): ResourceSnapshot<DataRecord[]> {
        return this.snapshot;
    }

    getServerSnapshot(): ResourceSnapshot<DataRecord[]> {
        return initialResourceSnapshot();
    }

    getRecords(): DataRecord[] | null {
        return this.snapshot.data;
    }

    subscribe(callback: (records: DataRecord[] | null) => void): () => void {
        // The first ref-counted consumer takes ownership from the imperative
        // API. Its final release can then close the server resource normally.
        this.imperativelyOwned = false;
        this.retainInStore();
        const shouldStart = this.trackedSubscriberCount() === 0;
        const subscriberId = this.nextSubscriberId++;
        this.subscriberEntries.set(subscriberId, callback);
        this.subscribers.push(callback);
        this.disposalToken++;
        if (shouldStart) {
            void this.start(false).catch(() => {
                // The error is part of the immutable snapshot and is surfaced by React.
            });
        }
        // Do not synchronously expose CONNECT/null lifecycle changes through
        // the legacy record callback. A later subscriber to an already-live
        // shared resource still receives its current server value immediately.
        if (this.snapshot.status === 'live' && this.snapshot.data !== null) {
            this.callSubscriber(callback, this.snapshot.data);
        }

        let subscribed = true;
        return () => {
            if (!subscribed) {
                return;
            }
            subscribed = false;
            this.subscriberEntries.delete(subscriberId);
            // subscribe() appends, so remove from the end to avoid consuming a
            // matching callback that legacy code inserted directly beforehand.
            const publicIndex = this.subscribers.lastIndexOf(callback);
            if (publicIndex !== -1) {
                this.subscribers.splice(publicIndex, 1);
            }
            this.scheduleCloseIfNotUsed();
        };
    }

    /** Internal external-store subscription; observes every snapshot identity change. */
    subscribeSnapshot(callback: () => void): () => void {
        this.imperativelyOwned = false;
        this.retainInStore();
        const shouldStart = this.trackedSubscriberCount() === 0;
        const subscriberId = this.nextSnapshotSubscriberId++;
        this.snapshotSubscriberEntries.set(subscriberId, callback);
        this.disposalToken++;
        if (shouldStart) {
            void this.start(false).catch(() => {
                // The error is part of the immutable snapshot and is surfaced by React.
            });
        }

        let subscribed = true;
        return () => {
            if (!subscribed) {
                return;
            }
            subscribed = false;
            this.snapshotSubscriberEntries.delete(subscriberId);
            this.scheduleCloseIfNotUsed();
        };
    }

    /** Starts a manually managed subscription. React and QueryBuilder do not need this. */
    async createOnServer(): Promise<void> {
        this.retainInStore();
        if (this.trackedSubscriberCount() === 0) {
            this.imperativelyOwned = true;
        }
        await this.start(false);
        // Scope-change and explicit-close paths settle the same public promise;
        // imperative callers must not observe a successful create when the
        // server resource was discarded before its first snapshot.
        await this.createOnServerPromise;
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
            this.closeForTransportScopeChange('before the subscription could commit');
            return Promise.resolve();
        }

        const controller = DataSyncController.getInstance();
        if (!controller.isBoundToTransportScope(this.expectedTransportScopeKey)) {
            this.closeForTransportScopeChange('before the subscription could acquire its controller');
            return Promise.resolve();
        }
        // A render may retain an object that was fully released between render
        // and commit. Reopening is supported, and its next final release must
        // notify the store again instead of leaving a closed registry entry.
        this.closeNotificationSent = false;
        this.attachControllerListeners(controller);
        this.started = true;
        const generation = ++this.generation;
        this.lastRevision = -1;
        this.supportsAuthoritativeSnapshots = false;
        this.dispatch({ type: 'CONNECT', reconnect });

        const startPromise = this.createServerSubscription(generation, controller);
        this.startPromise = startPromise;
        void startPromise.finally(() => {
            if (this.startPromise === startPromise) {
                this.startPromise = null;
            }
        }).catch(() => {});
        return startPromise;
    }

    private async createServerSubscription(generation: number, controller: DataSyncController): Promise<void> {
        try {
            const response = await controller.sendMessage({
                tag: 'CreateDataSubscription',
                query: this.serverQuery,
                protocolVersion: 1,
            });
            const subscriptionId = response.subscriptionId as UUID;

            if (!this.started
                || generation !== this.generation
                || this.controller !== controller
                || !this.hasExpectedTransportScope()
                || !controller.isBoundToTransportScope(this.expectedTransportScopeKey)) {
                if (!this.hasExpectedTransportScope()) {
                    this.closeForTransportScopeChange('while the subscription was being created');
                }
                await this.deleteStaleDataSubscription(controller, subscriptionId);
                return;
            }

            this.subscriptionId = subscriptionId;
            this.supportsAuthoritativeSnapshots = response.tag === 'DidCreateDataSubscriptionV2';
            const result = response.result as DataRecord[];
            // The V2 tag explicitly acknowledges snapshot support. An old
            // DidCreate response remains delta-compatible even when that server
            // silently ignored the protocolVersion request field.
            const initialRevision = this.supportsAuthoritativeSnapshots && typeof response.revision === 'number'
                ? response.revision
                : 0;
            this.applyServerSnapshot(result, initialRevision);
            this.resolveInitialCreateOnce(result);
        } catch (unknownError) {
            if (!this.started || generation !== this.generation || this.controller !== controller) {
                return;
            }

            const error = unknownError instanceof Error ? unknownError : new Error(String(unknownError));
            const connectionError = new Error(error.message + ' while trying to subscribe to:\n' + JSON.stringify(this.serverQuery, null, 4));
            this.dispatch({ type: 'FAIL', error: connectionError });
            this.rejectInitialCreateOnce(connectionError);
            throw connectionError;
        }
    }

    private attachControllerListeners(controller: DataSyncController): void {
        if (this.listenersAttached && this.controller === controller) {
            return;
        }
        this.detachControllerListeners();
        this.controller = controller;
        this.listenersAttached = true;
        controller.addEventListener('message', this.onMessage);
        controller.addEventListener('close', this.onDataSyncClosed);
        controller.addEventListener('reconnect', this.onDataSyncReconnect);
        if (!controller.dataSubscriptions.includes(this)) {
            controller.dataSubscriptions.push(this);
        }
    }

    private detachControllerListeners(): void {
        const controller = this.controller;
        if (controller === null) {
            this.listenersAttached = false;
            return;
        }
        if (this.listenersAttached) {
            controller.removeEventListener('message', this.onMessage);
            controller.removeEventListener('close', this.onDataSyncClosed);
            controller.removeEventListener('reconnect', this.onDataSyncReconnect);
            this.listenersAttached = false;
        }
        const index = controller.dataSubscriptions.indexOf(this);
        if (index !== -1) {
            controller.dataSubscriptions.splice(index, 1);
        }
        this.controller = null;
    }

    onMessage(message: ServerMessage): void {
        if (!this.started || message.subscriptionId !== this.subscriptionId) {
            return;
        }
        this.receiveUpdate(message);
    }

    receiveUpdate(message: ServerMessage): void {
        if (!this.hasExpectedTransportScope()) {
            this.closeForTransportScopeChange('before a server update was applied');
            return;
        }
        if (message.tag === 'DidReplaceDataSubscription') {
            const isFirstReplacement = !this.supportsAuthoritativeSnapshots;
            this.supportsAuthoritativeSnapshots = true;
            const revision = typeof message.revision === 'number'
                ? message.revision
                : this.lastRevision + 1;
            if (isFirstReplacement && revision <= this.lastRevision) {
                this.lastRevision = revision - 1;
            }
            this.applyServerSnapshot(message.result as DataRecord[], revision);
            return;
        }

        // Compatibility with older servers: delta messages are invalidations,
        // never instructions to emulate PostgreSQL query semantics in the browser.
        if (!this.supportsAuthoritativeSnapshots
            && (message.tag === 'DidInsert' || message.tag === 'DidUpdate' || message.tag === 'DidDelete')) {
            this.requestAuthoritativeRefresh();
        }
    }

    private applyServerSnapshot(records: DataRecord[], revision: number): void {
        if (revision <= this.lastRevision) {
            return;
        }
        this.lastRevision = revision;
        // Keep the public mutable `DataRecord[]` contract. The snapshot wrapper
        // is immutable, while callers remain free to use normal array methods.
        const snapshotRecords = records.map(record => ({ ...record })) as DataRecord[];
        if (this.scopedCache !== null
            && this.hasExpectedTransportScope()
            && this.controller?.isBoundToTransportScope(this.expectedTransportScopeKey) === true) {
            this.scopedCache.delete(this.cacheKey);
            this.scopedCache.set(
                this.cacheKey,
                snapshotRecords.map(record => ({ ...record })) as DataRecord[],
            );
            trimOldestMapEntries(this.scopedCache, 100);
        }
        this.dispatch({ type: 'SNAPSHOT', data: snapshotRecords });
    }

    private requestAuthoritativeRefresh(): void {
        if (!this.started) {
            return;
        }
        this.refreshRequested = true;
        if (this.refreshPromise !== null) {
            return;
        }
        const generation = this.generation;
        const refreshPromise = this.refreshUntilClean(generation);
        this.refreshPromise = refreshPromise;
        void refreshPromise.finally(() => {
            if (this.refreshPromise === refreshPromise) {
                this.refreshPromise = null;
            }
        }).catch(() => {});
    }

    private async refreshUntilClean(generation: number): Promise<void> {
        do {
            this.refreshRequested = false;
            if (this.supportsAuthoritativeSnapshots) {
                return;
            }
            try {
                const controller = this.controller;
                if (controller === null
                    || !this.hasExpectedTransportScope()
                    || !controller.isBoundToTransportScope(this.expectedTransportScopeKey)) {
                    if (!this.hasExpectedTransportScope()) {
                        this.closeForTransportScopeChange('before a legacy refresh was sent');
                    }
                    return;
                }
                const response = await controller.sendMessage({
                    tag: 'DataSyncQuery',
                    query: this.serverQuery,
                    transactionId: null,
                });
                // A replacement received while this legacy refetch was in
                // flight upgrades the resource to revisioned snapshots. The
                // unrevisioned query response can then be older and must not
                // overwrite that authoritative replacement.
                if (this.started
                    && generation === this.generation
                    && !this.supportsAuthoritativeSnapshots) {
                    this.applyServerSnapshot(response.result as DataRecord[], this.lastRevision + 1);
                }
            } catch (error) {
                if (this.started && generation === this.generation) {
                    console.error('Failed to refresh a legacy DataSubscription:', error);
                }
            }
        } while (this.refreshRequested && this.started && generation === this.generation);
    }

    onDataSyncClosed(event: unknown = null): void {
        if (!this.started) {
            return;
        }
        if (isTransportScopeChange(event)) {
            this.closeForTransportScopeChange('because the controller transport changed');
            return;
        }
        this.generation++;
        this.subscriptionId = null;
        this.lastRevision = -1;
        this.startPromise = null;
        this.refreshPromise = null;
        this.refreshRequested = false;
        this.dispatch({ type: 'DISCONNECT' });
    }

    async onDataSyncReconnect(): Promise<void> {
        if (!this.started || this.startPromise !== null || this.subscriptionId !== null) {
            return;
        }
        await this.start(true).catch(() => {
            // The failure is observable through getSnapshot().
        });
    }

    /** @deprecated Delta mutation now triggers an exact server refresh. */
    onUpdate(
        _id: UUID,
        _changeSet: Record<string, unknown> | null,
        _appendSet: Record<string, unknown> | null,
        _isOptimistic = false,
    ): void {
        this.requestAuthoritativeRefresh();
    }

    /** @deprecated Delta mutation now triggers an exact server refresh. */
    onCreate(_newRecord: DataRecord, _isOptimistic = false): void {
        this.requestAuthoritativeRefresh();
    }

    /** @deprecated Optimistic mutation is disabled and triggers an exact refresh. */
    onCreateOptimistic(newRecord: DataRecord): void {
        if (!('id' in newRecord)) {
            throw new Error('Requires the record to have an id');
        }
        this.requestAuthoritativeRefresh();
    }

    /** @deprecated Delta mutation now triggers an exact server refresh. */
    onDelete(_id: UUID, _isOptimistic = false): void {
        this.requestAuthoritativeRefresh();
    }

    /** @deprecated Optimistic updates are intentionally disabled. */
    supportsOptimisticUpdates(): boolean {
        return false;
    }

    /** @deprecated Snapshots notify automatically; retained for compatibility. */
    updateSubscribers(): void {
        this.notifySubscribers();
    }

    scheduleCloseIfNotUsed(): void {
        const token = ++this.disposalToken;
        queueMicrotask(() => {
            if (token === this.disposalToken) {
                this.closeIfNotUsed();
            }
        });
    }

    closeIfNotUsed(): void {
        if (this.subscribers.length !== 0
            || this.snapshotSubscriberEntries.size !== 0
            || this.imperativelyOwned) {
            return;
        }
        void this.stop().catch(error => {
            console.error('Failed to close an unused DataSubscription:', error);
        });
    }

    async close(): Promise<void> {
        this.imperativelyOwned = false;
        this.subscriberEntries.clear();
        this.snapshotSubscriberEntries.clear();
        this.subscribers.length = 0;
        this.disposalToken++;
        await this.stop();
    }

    private async stop(): Promise<void> {
        if (!this.started && this.snapshot.status === 'closed') {
            return;
        }

        this.started = false;
        this.generation++;
        this.startPromise = null;
        this.refreshPromise = null;
        this.refreshRequested = false;
        const subscriptionId = this.subscriptionId;
        const controller = this.controller;
        this.subscriptionId = null;
        this.detachControllerListeners();
        this.dispatch({ type: 'CLOSE' });
        this.rejectInitialCreateOnce(new Error('DataSubscription closed before its initial server snapshot arrived'));
        this.notifyClose();

        if (subscriptionId !== null && controller !== null) {
            await this.deleteStaleDataSubscription(controller, subscriptionId);
        }
    }

    private notifyClose(): void {
        if (this.closeNotificationSent) {
            return;
        }
        this.closeNotificationSent = true;
        try {
            this.onStoreClose();
        } catch (error) {
            console.error('DataSubscription store-close listener failed:', error);
        }
        try {
            this.onClose();
        } catch (error) {
            console.error('DataSubscription close listener failed:', error);
        }
    }

    private async deleteStaleDataSubscription(controller: DataSyncController, subscriptionId: UUID): Promise<void> {
        try {
            await controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId });
        } catch (error) {
            if (!controller.retired && controller.connection !== null) {
                console.error('Failed to delete a stale DataSubscription:', error);
            }
        }
    }

    private closeForTransportScopeChange(reason: string): void {
        if (!this.started && this.snapshot.status === 'closed' && this.closeNotificationSent) {
            return;
        }
        this.imperativelyOwned = false;
        this.started = false;
        this.generation++;
        this.subscriptionId = null;
        this.lastRevision = -1;
        this.startPromise = null;
        this.refreshPromise = null;
        this.refreshRequested = false;
        this.supportsAuthoritativeSnapshots = false;
        this.detachControllerListeners();
        this.snapshot = initialResourceSnapshot();
        this.dispatch({ type: 'CLOSE' });
        this.rejectInitialCreateOnce(new Error(`DataSubscription closed because its authentication/backend scope changed ${reason}`));
        this.notifyClose();
    }

    private hasExpectedTransportScope(): boolean {
        return this.expectedTransportScopeKey === DataSyncController.currentTransportScopeKey();
    }

    private resolveInitialCreateOnce(records: DataRecord[]): void {
        if (this.initialCreateSettled) {
            return;
        }
        this.initialCreateSettled = true;
        this.resolveInitialCreate(records);
    }

    private rejectInitialCreateOnce(error: Error): void {
        if (this.initialCreateSettled) {
            return;
        }
        this.initialCreateSettled = true;
        this.rejectInitialCreate(error);
    }

    private dispatch(action: ResourceSnapshotAction<DataRecord[]>): void {
        const nextSnapshot = reduceResourceSnapshot(this.snapshot, action);
        if (nextSnapshot === this.snapshot) {
            return;
        }
        this.snapshot = nextSnapshot;
        switch (action.type) {
            case 'CONNECT':
                this.isConnected = false;
                if (!action.reconnect) {
                    this.isClosed = false;
                }
                this.connectError = null;
                break;
            case 'SNAPSHOT':
                this.isClosed = false;
                this.isConnected = true;
                this.connectError = null;
                break;
            case 'DISCONNECT':
                this.isClosed = true;
                this.isConnected = false;
                break;
            case 'FAIL':
                this.isConnected = false;
                this.connectError = action.error;
                break;
            case 'CLOSE':
                this.isClosed = true;
                this.isConnected = false;
                this.connectError = null;
                break;
        }
        this.notifySnapshotSubscribers();
        if (action.type === 'SNAPSHOT' || action.type === 'FAIL') {
            this.notifySubscribers();
        }
    }

    private trackedSubscriberCount(): number {
        return this.subscriberEntries.size + this.snapshotSubscriberEntries.size;
    }

    private retainInStore(): void {
        // Never promote a stale render handle back into the active registry.
        // Its commit-time snapshot check will make React resolve a resource for
        // the current scope instead.
        if (!this.hasExpectedTransportScope()) {
            return;
        }
        try {
            this.onStoreRetain();
        } catch (error) {
            console.error('DataSubscription store-retain listener failed:', error);
        }
    }

    private notifySnapshotSubscribers(): void {
        for (const subscriber of Array.from(this.snapshotSubscriberEntries.values())) {
            try {
                subscriber();
            } catch (error) {
                console.error('DataSubscription snapshot subscriber failed:', error);
            }
        }
    }

    private notifySubscribers(): void {
        for (const subscriber of this.subscribers.slice()) {
            this.callSubscriber(subscriber, this.snapshot.data);
        }
    }

    private callSubscriber(
        subscriber: (records: DataRecord[] | null) => void,
        records: DataRecord[] | null,
    ): void {
        try {
            subscriber(records);
        } catch (error) {
            console.error('DataSubscription subscriber failed:', error);
        }
    }

    /** @deprecated Server snapshots make local record placement irrelevant. */
    detectNewRecordBehaviour(): number {
        const firstOrderBy = this.query.orderByClause[0];
        return firstOrderBy
            && 'orderByColumn' in firstOrderBy
            && firstOrderBy.orderByColumn === 'createdAt'
            && firstOrderBy.orderByDirection === 'Desc'
            ? PREPEND_NEW_RECORD
            : APPEND_NEW_RECORD;
    }
}

function cloneQuery(query: DynamicSQLQuery): DynamicSQLQuery {
    return JSON.parse(JSON.stringify(query)) as DynamicSQLQuery;
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

function trimOldestMapEntries<K, V>(map: Map<K, V>, maximumSize: number): void {
    while (map.size > maximumSize) {
        const oldestKey = map.keys().next().value as K | undefined;
        if (oldestKey === undefined) {
            return;
        }
        map.delete(oldestKey);
    }
}

function initIHPBackend({ host }: { host: string }): void {
    if (typeof host !== "string" || (!host.startsWith("http://") && !host.startsWith("https://"))) {
        throw new Error("IHP Backend host url needs to start with \"http://\" or \"https://\", you passed \"" + host + "\"");
    }
    if (host.endsWith('/')) {
        throw new Error('IHP Backend host url should not have a trailing slash, please remove the last "/" from "' + host + '"');
    }
    const previousHost = DataSyncController.ihpBackendHost;
    DataSyncController.ihpBackendHost = host;
    // Configuration is an explicit imperative operation. Rotate an already
    // existing controller now; a later render-only store lookup stays inert.
    if (DataSyncController.instance !== null && previousHost !== host) {
        DataSyncController.retireCurrentTransport();
    }
}

export async function createRecord<T extends TableName>(
    table: T,
    record: NewRecord<T>,
    options: CrudOptions = {},
    boundController?: DataSyncController,
): Promise<IHPRecord<T>> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to createRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }
    if (record !== Object(record)) {
        throw new Error(`Record needs to be an object, you passed ${JSON.stringify(record)} in a call to createRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }
    const dataSyncController = boundController ?? DataSyncController.getInstance();

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'CreateRecordMessage', table, record, transactionId };
    const coordinatesDependentWrites = transactionId === null;

    if (coordinatesDependentWrites) {
        if (record.id == null) {
            record.id = randomUUID();
        }
        registerPendingCreate(dataSyncController, record.id);
    }

    try {
        await waitPendingChanges(dataSyncController, table, record);
        const response = await dataSyncController.sendMessage(request);
        if (coordinatesDependentWrites) {
            finishPendingCreate(dataSyncController, record.id!, null);
        }
        return response.record as IHPRecord<T>;
    } catch (e) {
        if (coordinatesDependentWrites) {
            finishPendingCreate(dataSyncController, record.id!, e as Error);
        }

        throw new Error(`${(e as Error).message} while calling:\n\ncreateRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }
}

export async function updateRecord<T extends TableName>(
    table: T,
    id: UUID,
    patch: Partial<NewRecord<T>>,
    options: CrudOptions = {},
    boundController?: DataSyncController,
): Promise<IHPRecord<T>> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to updateRecord(${JSON.stringify(table)}, ${JSON.stringify(id)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (typeof id !== "string") {
        throw new Error(`ID needs to be an UUID, you passed ${JSON.stringify(id)} in a call to updateRecord(${JSON.stringify(table)}, ${JSON.stringify(id)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (patch !== Object(patch)) {
        throw new Error(`Patch needs to be an object, you passed ${JSON.stringify(patch)} in a call to updateRecord(${JSON.stringify(table)}, ${JSON.stringify(id)}, ${JSON.stringify(patch, null, 4)})`);
    }
    const dataSyncController = boundController ?? DataSyncController.getInstance();

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'UpdateRecordMessage', table, id, patch, transactionId };

    try {
        await waitPendingCreation(dataSyncController, table, id);
        await waitPendingChanges(dataSyncController, table, patch);
        const response = await dataSyncController.sendMessage(request);

        return response.record as IHPRecord<T>;
    } catch (e) {
        throw new Error((e as Error).message);
    }
}

export async function updateRecords<T extends TableName>(
    table: T,
    ids: UUID[],
    patch: Partial<NewRecord<T>>,
    options: CrudOptions = {},
    boundController?: DataSyncController,
): Promise<IHPRecord<T>[]> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to updateRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (!Array.isArray(ids)) {
        throw new Error(`IDs need to be an array, you passed ${JSON.stringify(ids)} in a call to updateRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (patch !== Object(patch)) {
        throw new Error(`Patch needs to be an object, you passed ${JSON.stringify(patch)} in a call to updateRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)}, ${JSON.stringify(patch, null, 4)})`);
    }
    const dataSyncController = boundController ?? DataSyncController.getInstance();

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'UpdateRecordsMessage', table, ids, patch, transactionId };

    try {
        const response = await dataSyncController.sendMessage(request);

        return response.records as IHPRecord<T>[];
    } catch (e) {
        throw new Error((e as Error).message);
    }
}

export async function deleteRecord<T extends TableName>(
    table: T,
    id: UUID,
    options: CrudOptions = {},
    boundController?: DataSyncController,
): Promise<void> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to deleteRecord(${JSON.stringify(table)}, ${JSON.stringify(id)})`);
    }
    if (typeof id !== "string") {
        throw new Error(`ID needs to be an UUID, you passed ${JSON.stringify(id)} in a call to deleteRecord(${JSON.stringify(table)}, ${JSON.stringify(id)})`);
    }
    const dataSyncController = boundController ?? DataSyncController.getInstance();

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'DeleteRecordMessage', table, id, transactionId };

    try {
        await waitPendingCreation(dataSyncController, table, id);
        await dataSyncController.sendMessage(request);

        return;
    } catch (e) {
        throw new Error((e as Error).message);
    }
}

export async function deleteRecords<T extends TableName>(
    table: T,
    ids: UUID[],
    options: CrudOptions = {},
    boundController?: DataSyncController,
): Promise<void> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to deleteRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)})`);
    }
    if (!Array.isArray(ids)) {
        throw new Error(`IDs needs to be an array, you passed ${JSON.stringify(ids)} in a call to deleteRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)})`);
    }
    const dataSyncController = boundController ?? DataSyncController.getInstance();

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'DeleteRecordsMessage', table, ids, transactionId };

    try {
        await dataSyncController.sendMessage(request);

        return;
    } catch (e) {
        throw new Error((e as Error).message);
    }
}

export async function createRecords<T extends TableName>(
    table: T,
    records: NewRecord<T>[],
    options: CrudOptions = {},
    boundController?: DataSyncController,
): Promise<IHPRecord<T>[]> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to createRecords(${JSON.stringify(table)}, ${JSON.stringify(records, null, 4)})`);
    }
    if (!Array.isArray(records)) {
        throw new Error(`Records need to be an array, you passed ${JSON.stringify(records)} in a call to createRecords(${JSON.stringify(table)}, ${JSON.stringify(records, null, 4)})`);
    }
    const dataSyncController = boundController ?? DataSyncController.getInstance();

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'CreateRecordsMessage', table, records, transactionId };

    try {
        const response = await dataSyncController.sendMessage(request);

        return response.records as IHPRecord<T>[];
    } catch (e) {
        throw new Error((e as Error).message);
    }
}

function registerPendingCreate(dataSyncController: DataSyncController, id: UUID): void {
    if (dataSyncController.pendingCreates.has(id)) {
        return;
    }

    let resolve!: () => void;
    let reject!: (reason: Error) => void;
    const promise = new Promise<void>((resolvePromise, rejectPromise) => {
        resolve = resolvePromise;
        reject = rejectPromise;
    });
    // Most creates have no dependent operation. Mark rejection as observed while
    // keeping the same promise rejectable for writes that reference this ID.
    void promise.catch(() => {});
    dataSyncController.pendingCreates.set(id, { promise, resolve, reject });
}

function randomUUID(): UUID {
    // Some older browsers like firefox 91 ESR don't support crypto.randomUUID
    // So we have a fallback to keep the app working in these browsers
    try {
        return crypto.randomUUID();
    } catch (e) {
        // https://stackoverflow.com/a/873856/14144232
        // http://www.ietf.org/rfc/rfc4122.txt
        const s: string[] = [];
        const hexDigits = "0123456789abcdef";
        for (let i = 0; i < 36; i++) {
            s[i] = hexDigits.substr(Math.floor(Math.random() * 0x10), 1);
        }
        s[14] = "4";  // bits 12-15 of the time_hi_and_version field to 0010
        s[19] = hexDigits.substr((parseInt(s[19], 16) & 0x3) | 0x8, 1);  // bits 6-7 of the clock_seq_hi_and_reserved to 01
        s[8] = s[13] = s[18] = s[23] = "-";

        return s.join("");
    }
}

function finishPendingCreate(dataSyncController: DataSyncController, id: UUID, error: Error | null): void {
    const pendingCreate = dataSyncController.pendingCreates.get(id);
    if (pendingCreate) {
        dataSyncController.pendingCreates.delete(id);
        if (error) {
            pendingCreate.reject(error);
        } else {
            pendingCreate.resolve();
        }
    }
}

function pendingCreatesReferencedBy<T extends TableName>(
    dataSyncController: DataSyncController,
    record: NewRecord<T> | Partial<NewRecord<T>>,
): Promise<void>[] {
    const rec = record as Record<string, unknown>;
    const pendingCreates = new Set<Promise<void>>();

    for (const attribute in rec) {
        if (attribute === 'id') {
            continue; // Never treat the record's own id as a create dependency.
        }
        const pendingCreate = dataSyncController.pendingCreates.get(rec[attribute] as UUID);
        if (pendingCreate) {
            pendingCreates.add(pendingCreate.promise);
        }
    }

    return Array.from(pendingCreates);
}

async function waitPendingChanges<T extends TableName>(
    dataSyncController: DataSyncController,
    _table: T,
    record: NewRecord<T> | Partial<NewRecord<T>>,
): Promise<void> {
    await Promise.all(pendingCreatesReferencedBy(dataSyncController, record));
}

async function waitPendingCreation<T extends TableName>(
    dataSyncController: DataSyncController,
    _table: T,
    id: UUID,
): Promise<void> {
    const pendingCreate = dataSyncController.pendingCreates.get(id);
    if (pendingCreate) {
        await pendingCreate.promise;
    }
}

export { DataSyncController, DataSubscription, initIHPBackend, NewRecordBehaviour };
