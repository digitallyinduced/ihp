import { recordMatchesQuery } from './ihp-querybuilder.js';
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

const UNUSED_SUBSCRIPTION_CLOSE_DELAY = 1000;

type EventListeners = {
    [K in DataSyncEventType]: DataSyncEventMap[K][];
};

type OutboxMessage = {
    requestId: number;
    payload: string;
};

type PendingOptimisticCreate = {
    promise: Promise<void>;
    resolve: () => void;
    reject: (reason: Error) => void;
};

class DataSyncController {
    static instance: DataSyncController | null = null;
    static ihpBackendHost: string | null = null;

    static getInstance(): DataSyncController {
        if (!DataSyncController.instance) {
            DataSyncController.instance = new DataSyncController();
        }

        return DataSyncController.instance;
    }

    static getWSUrl(): string {
        if (DataSyncController.ihpBackendHost) {
            const jwt = localStorage.getItem('ihp_jwt');
            const host = DataSyncController.ihpBackendHost
                .replace('https://', 'wss://')
                .replace('http://', 'ws://');
            return host + '/DataSyncController' + (jwt !== null ? '?access_token=' + encodeURIComponent(jwt) : '');
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
    optimisticCreatedPendingRecordIds: UUID[];
    pendingOptimisticCreates: Map<UUID, PendingOptimisticCreate>;
    optimisticCreatedNeedsCreatedAtField: Set<string>;
    messageTimeout: number;
    connectionRetryLimit: number;
    connectionRetryMaxDelayExponent: number;
    pendingConnection: Promise<WebSocket> | null;

    constructor() {
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
        this.optimisticCreatedPendingRecordIds = [];
        this.pendingOptimisticCreates = new Map();
        this.optimisticCreatedNeedsCreatedAtField = new Set();
        this.messageTimeout = 5000;
        this.connectionRetryLimit = 32;
        this.connectionRetryMaxDelayExponent = 6;
        this.pendingConnection = null;
    }

    async startConnection(): Promise<WebSocket> {
        if (this.connection) {
            return this.connection;
        }

        if (this.pendingConnection) {
            return await this.pendingConnection;
        }

        let pendingConnection!: Promise<WebSocket>;
        pendingConnection = (async () => {
            const connect = (): Promise<{ socket: WebSocket; event: Event }> => new Promise((resolve, reject) => {
                const socket = new WebSocket(DataSyncController.getWSUrl());

                socket.onopen = (event) => {
                    this.connection = socket;
                    socket.onclose = (closeEvent) => this.onClose(closeEvent, socket);
                    socket.onmessage = this.onMessage.bind(this);
                    resolve({ socket, event });
                };

                socket.onerror = (event) => reject(event);
            });
            const wait = (timeout: number): Promise<void> => new Promise((resolve) => setTimeout(resolve, timeout));
            try {
                for (let i = 0; i < this.connectionRetryLimit; i++) {
                    try {
                        const { socket, event } = await connect();
                        if (this.connection !== socket) {
                            throw new Error('DataSync WebSocket closed while the connection was opening');
                        }
                        this.flushOutbox(socket);

                        for (const listener of this.eventListeners.open) {
                            try {
                                listener(event);
                            } catch (error) {
                                console.error('DataSync open listener failed:', error);
                            }
                        }

                        return socket;
                    } catch (error) {
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
        this.eventListeners.message.slice(0).forEach(callback => callback(payload));
    }

    onClose(_event: CloseEvent | null, closedSocket: WebSocket | null = null): void {
        if (closedSocket !== null && this.connection !== closedSocket) {
            return;
        }

        this.connection = null;
        this.rejectSentPendingRequests(new Error('DataSync WebSocket closed before the server responded'));

        for (const listener of this.eventListeners.close) {
            listener(_event);
        }

        this.retryToReconnect();
    }

    async sendMessage(payload: Record<string, unknown>): Promise<ServerMessage> {
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
        if (this.connection) {
            return;
        }

        if (this.reconnectTimeout) {
            clearTimeout(this.reconnectTimeout);
        }
        this.reconnectTimeout = setTimeout(async () => {
            this.reconnectTimeout = null;
            try {
                console.log('Trying to reconnect DataSync ...');
                await this.startConnection();

                for (const listener of this.eventListeners.reconnect) {
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

    learnOptimisticShapeFromResult(table: string, result: DataRecord[]): void {
        if (result.length > 0) {
            const hasCreatedAtField = 'createdAt' in result[0];
            if (hasCreatedAtField) {
                this.optimisticCreatedNeedsCreatedAtField.add(table);
            }
        }
    }

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
    createOnServerPromise: Promise<DataRecord[]>;
    resolveCreateOnServer!: (value: DataRecord[]) => void;
    rejectCreateOnServer!: (reason: Error) => void;
    isClosed: boolean;
    isConnected: boolean;
    connectError: Error | null;
    subscriptionId: UUID | null;
    subscribers: Array<(records: DataRecord[] | null) => void>;
    records: DataRecord[] | null;
    cache: Map<string, DataRecord[]> | null;
    newRecordBehaviour: number;
    optimisticCreatedPendingRecordIds: UUID[];
    optimisticUpdatedPendingRecordIds: Set<UUID>;
    private closeNotificationSent: boolean;
    private closeIfNotUsedTimeout: ReturnType<typeof setTimeout> | null;
    private refreshPromise: Promise<void> | null;
    private refreshRequested: boolean;

    constructor(query: DynamicSQLQuery, options: DataSubscriptionOptions | null = null, cache: Map<string, DataRecord[]> | null = null) {
        if (typeof query !== "object" || !('table' in query)) {
            throw new Error("Query passed to `new DataSubscription(..)` doesn't look like a query object. If you're using the `query()` functions to costruct the object, make sure you pass the `.query` property, like this: `new DataSubscription(query('my_table').orderBy('createdAt').query)`");
        }
        this.query = query;
        this.createOnServerPromise = new Promise((resolve, reject) => {
            this.resolveCreateOnServer = resolve;
            this.rejectCreateOnServer = reject;
        });
        void this.createOnServerPromise.catch(() => {
            // close() observes the same promise when it needs to wait for setup;
            // most subscriptions never need that path, so avoid an unhandled rejection.
        });

        this.isClosed = false;
        this.isConnected = false;
        this.connectError = null;
        this.subscriptionId = null;
        this.subscribers = [];

        if (cache) {
            const cacheResults = cache.get(JSON.stringify(query));
            if (cacheResults !== undefined) {
                this.records = cacheResults;
            } else {
                this.records = null;
            }
        } else {
            this.records = null;
        }
        this.cache = cache;

        this.getRecords = this.getRecords.bind(this);
        this.subscribe = this.subscribe.bind(this);
        this.onDataSyncClosed = this.onDataSyncClosed.bind(this);
        this.onDataSyncReconnect = this.onDataSyncReconnect.bind(this);
        this.onMessage = this.onMessage.bind(this);

        // When a new record is inserted, do we put it at the end or at the beginning?
        this.newRecordBehaviour = (options && 'newRecordBehaviour' in options) ? options.newRecordBehaviour! : this.detectNewRecordBehaviour();

        this.optimisticCreatedPendingRecordIds = [];
        this.optimisticUpdatedPendingRecordIds = new Set();
        this.closeNotificationSent = false;
        this.closeIfNotUsedTimeout = null;
        this.refreshPromise = null;
        this.refreshRequested = false;
    }

    detectNewRecordBehaviour(): number {
        // If the query is ordered by the createdAt column, and the latest is at the top
        // we want to prepend new record
        const firstOrderBy = this.query.orderByClause[0];
        const isOrderByCreatedAtDesc = this.query.orderByClause.length > 0
            && firstOrderBy
            && 'orderByColumn' in firstOrderBy
            && firstOrderBy.orderByColumn === 'createdAt'
            && firstOrderBy.orderByDirection === 'Desc';

        if (isOrderByCreatedAtDesc) {
            return PREPEND_NEW_RECORD;
        }

        return APPEND_NEW_RECORD;
    }

    async createOnServer(): Promise<void> {
        const dataSyncController = DataSyncController.getInstance();
        try {
            const response = await dataSyncController.sendMessage({ tag: 'CreateDataSubscription', query: this.query });
            const subscriptionId = response.subscriptionId as UUID;
            const result = response.result as DataRecord[];

            this.subscriptionId = subscriptionId;

            // This condition ensure that the event listeners are only installed on first
            // run. This function could be called multiple times (e.g. a second time on internet reconnect).
            // In those cases we already did register the event listener.
            if (this.isClosed === false) {
                dataSyncController.addEventListener('message', this.onMessage);
                dataSyncController.addEventListener('close', this.onDataSyncClosed);
                dataSyncController.addEventListener('reconnect', this.onDataSyncReconnect);
                dataSyncController.dataSubscriptions.push(this);
            }

            this.isConnected = true;
            this.isClosed = false;
            this.connectError = null;
            this.records = this.normalizeRecords(result);

            this.resolveCreateOnServer(result);
            this.updateSubscribers();

            dataSyncController.learnOptimisticShapeFromResult(this.query.table, result);
        } catch (e) {
            const error = e as Error;
            this.isConnected = false;
            this.connectError = new Error(error.message + ' while trying to subscribe to:\n' + JSON.stringify(this.query, null, 4));
            this.rejectCreateOnServer(this.connectError);
            this.notifySubscribers();
            throw this.connectError;
        }
    }

    onMessage(message: ServerMessage): void {
        if (this.isClosed) {
            return;
        }
        if (message.subscriptionId === this.subscriptionId) {
            this.receiveUpdate(message);
        }
    }

    receiveUpdate(message: ServerMessage): void {
        const tag = message.tag;
        if (tag === 'DidUpdate') {
            this.onUpdate(message.id as UUID, message.changeSet as Record<string, unknown> | null, message.appendSet as Record<string, unknown> | null);
        } else if (tag === 'DidInsert') {
            this.onCreate(message.record as DataRecord);
        } else if (tag === 'DidDelete') {
            this.onDelete(message.id as UUID);
        }
    }

    async close(): Promise<void> {
        const dataSyncController = DataSyncController.getInstance();
        this.cancelScheduledCloseIfNotUsed();

        if (this.isClosed) {
            // A dropped WebSocket marks every subscription as closed. There is
            // no server-side subscription left to delete, but an unused React
            // subscription still needs to be removed from the local store and
            // reconnect list.
            this.notifyClose();
            this.detachFromDataSyncController(dataSyncController);
            return;
        }

        // We cannot close the DataSubscription when the subscriptionId is not assigned
        if (!this.isClosed && !this.isConnected) {
            try {
                await this.createOnServerPromise;
            } catch (_error) {
                this.isClosed = true;
                this.notifyClose();
                this.detachFromDataSyncController(dataSyncController);
                return;
            }
            return this.close();
        }

        // Set isClosed early as we need to prevent a second close() from triggering another DeleteDataSubscription message
        // also we don't want to receive any further messages, and onMessage will not process if isClosed == true
        this.isClosed = true;
        this.notifyClose();

        try {
            await dataSyncController.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId: this.subscriptionId });
        } finally {
            this.detachFromDataSyncController(dataSyncController);
        }
    }

    private detachFromDataSyncController(dataSyncController: DataSyncController): void {
        dataSyncController.removeEventListener('message', this.onMessage);
        dataSyncController.removeEventListener('close', this.onDataSyncClosed);
        dataSyncController.removeEventListener('reconnect', this.onDataSyncReconnect);
        const index = dataSyncController.dataSubscriptions.indexOf(this);
        if (index !== -1) {
            dataSyncController.dataSubscriptions.splice(index, 1);
        }

        this.isConnected = false;
    }

    private notifyClose(): void {
        if (this.closeNotificationSent) {
            return;
        }

        this.closeNotificationSent = true;
        this.onClose();
    }

    onDataSyncClosed(): void {
        this.isClosed = true;
        this.isConnected = false;

        // The controller reconnects after one second. This timeout is registered
        // before the reconnect timeout, so unused subscriptions are pruned first.
        // A React commit that subscribes in the meantime cancels the cleanup.
        this.scheduleCloseIfNotUsed();
    }

    onDataSyncReconnect(): void {
        void this.createOnServer().catch(() => {
            // createOnServer stores the error and notifies React subscribers.
        });
    }

    onUpdate(id: UUID, changeSet: Record<string, unknown> | null, appendSet: Record<string, unknown> | null): void {
        this.records = this.normalizeRecords(this.records!.map(record => {
            if (record.id === id) {
                const updated = Object.assign({}, record, changeSet);
                if (appendSet && !this.optimisticUpdatedPendingRecordIds.has(id)) {
                    for (const [key, value] of Object.entries(appendSet)) {
                        (updated as Record<string, unknown>)[key] = (typeof updated[key] === 'string' ? updated[key] : '') + String(value);
                    }
                }
                return updated;
            }

            return record;
        }));

        this.optimisticUpdatedPendingRecordIds.delete(id);
        this.updateSubscribers();
        this.refreshAfterComplexMutation();
    }

    onCreate(newRecord: DataRecord): void {
        const shouldAppend = this.newRecordBehaviour === APPEND_NEW_RECORD;

        const newRecordId = newRecord.id;
        const isOptimisticallyCreatedAlready = this.optimisticCreatedPendingRecordIds.indexOf(newRecordId) !== -1;
        if (isOptimisticallyCreatedAlready) {
            this.onUpdate(newRecordId, newRecord, null);
            this.optimisticCreatedPendingRecordIds.splice(this.optimisticCreatedPendingRecordIds.indexOf(newRecordId), 1);
            return;
        } else {
            const records = shouldAppend ? [...this.records!, newRecord] : [newRecord, ...this.records!];
            this.records = this.normalizeRecords(records);
        }

        this.updateSubscribers();
        this.refreshAfterComplexMutation();
    }

    onCreateOptimistic(newRecord: DataRecord): void {
        if (!('id' in newRecord)) {
            throw new Error('Requires the record to have an id');
        }

        this.onCreate(newRecord);
        this.optimisticCreatedPendingRecordIds.push(newRecord.id);
    }

    onDelete(id: UUID): void {
        this.records = this.records!.filter(record => record.id !== id);
        const optimisticIndex = this.optimisticCreatedPendingRecordIds.indexOf(id);
        if (optimisticIndex !== -1) {
            this.optimisticCreatedPendingRecordIds.splice(optimisticIndex, 1);
        }
        this.updateSubscribers();
        this.refreshAfterComplexMutation();
    }

    subscribe(callback: (records: DataRecord[] | null) => void): () => void {
        this.cancelScheduledCloseIfNotUsed();
        this.subscribers.push(callback);

        return () => {
            const index = this.subscribers.indexOf(callback);
            if (index !== -1) {
                this.subscribers.splice(index, 1);
            }

            // We delay the close as react could be re-rendering a component
            // we garbage collect this connecetion once it's clearly not used anymore
            this.scheduleCloseIfNotUsed();
        };
    }

    scheduleCloseIfNotUsed(): void {
        this.cancelScheduledCloseIfNotUsed();
        this.closeIfNotUsedTimeout = setTimeout(() => {
            this.closeIfNotUsedTimeout = null;
            this.closeIfNotUsed();
        }, UNUSED_SUBSCRIPTION_CLOSE_DELAY);
    }

    private cancelScheduledCloseIfNotUsed(): void {
        if (this.closeIfNotUsedTimeout !== null) {
            clearTimeout(this.closeIfNotUsedTimeout);
            this.closeIfNotUsedTimeout = null;
        }
    }

    updateSubscribers(): void {
        if (this.cache && this.records !== null) {
            this.cache.set(JSON.stringify(this.query), this.records);
        }
        this.notifySubscribers();
    }

    private notifySubscribers(): void {
        for (const subscriber of this.subscribers) {
            subscriber(this.records);
        }
    }

    private normalizeRecords(records: DataRecord[]): DataRecord[] {
        let normalized = [...records];
        const sortableClauses = this.query.orderByClause.filter(clause => 'orderByColumn' in clause);

        if (sortableClauses.length > 0) {
            normalized.sort((left, right) => {
                for (const clause of sortableClauses) {
                    const comparison = compareQueryValues(left[clause.orderByColumn], right[clause.orderByColumn]);
                    if (comparison !== 0) {
                        return clause.orderByDirection === 'Desc' ? -comparison : comparison;
                    }
                }
                return 0;
            });
        }

        if (this.query.distinctOnColumn !== null) {
            const seen = new Set<unknown>();
            normalized = normalized.filter(record => {
                const value = record[this.query.distinctOnColumn!];
                if (seen.has(value)) {
                    return false;
                }
                seen.add(value);
                return true;
            });
        }

        if (this.query.limit !== null && this.query.limit >= 0) {
            normalized = normalized.slice(0, this.query.limit);
        }

        return normalized;
    }

    private refreshAfterComplexMutation(): void {
        const selectedColumns = this.query.selectedColumns;
        const hasUnprojectedOrderColumn = selectedColumns.tag === 'SelectSpecific'
            && this.query.orderByClause.some(clause => 'orderByColumn' in clause
                && !selectedColumns.contents.includes(clause.orderByColumn));
        const requiresRefresh = this.query.limit !== null
            || this.query.offset !== null
            || this.query.distinctOnColumn !== null
            || this.query.orderByClause.some(clause => 'tag' in clause && clause.tag === 'OrderByTSRank')
            || hasUnprojectedOrderColumn;
        if (!requiresRefresh || this.isClosed) {
            return;
        }

        this.refreshRequested = true;
        if (this.refreshPromise !== null) {
            return;
        }

        this.refreshPromise = this.refreshRecordsFromServer();
    }

    private async refreshRecordsFromServer(): Promise<void> {
        this.refreshRequested = false;
        try {
            const response = await DataSyncController.getInstance().sendMessage({
                tag: 'DataSyncQuery',
                query: this.query,
                transactionId: null
            });
            if (!this.isClosed) {
                this.records = this.normalizeRecords(response.result as DataRecord[]);
                this.updateSubscribers();
            }
        } catch (error) {
            if (!this.isClosed) {
                console.error('Failed to refresh a complex DataSubscription:', error);
            }
        } finally {
            this.refreshPromise = null;
            if (this.refreshRequested) {
                this.refreshAfterComplexMutation();
            }
        }
    }

    getRecords(): DataRecord[] | null {
        return this.records;
    }

    /**
     * If there's no subscriber on this DataSubscription, we will close it.
     */
    closeIfNotUsed(): void {
        const isUsed = this.subscribers.length > 0;
        if (isUsed) {
            return;
        }

        this.close();
    }

    onClose(): void {
        // Overriden by the react 18 integration to remove the closed connection from the DataSubscriptionStore
    }
}

function compareQueryValues(left: unknown, right: unknown): number {
    if (Object.is(left, right)) {
        return 0;
    }
    // PostgreSQL's default is NULLS LAST for ASC and NULLS FIRST for DESC.
    if (left === null || left === undefined) {
        return 1;
    }
    if (right === null || right === undefined) {
        return -1;
    }

    const normalizedLeft = left instanceof Date ? left.getTime() : left;
    const normalizedRight = right instanceof Date ? right.getTime() : right;
    if ((typeof normalizedLeft === 'number' && typeof normalizedRight === 'number')
        || (typeof normalizedLeft === 'string' && typeof normalizedRight === 'string')
        || (typeof normalizedLeft === 'boolean' && typeof normalizedRight === 'boolean')) {
        return normalizedLeft < normalizedRight ? -1 : 1;
    }

    const leftText = String(normalizedLeft);
    const rightText = String(normalizedRight);
    return leftText < rightText ? -1 : leftText > rightText ? 1 : 0;
}

function initIHPBackend({ host }: { host: string }): void {
    if (typeof host !== "string" || (!host.startsWith("http://") && !host.startsWith("https://"))) {
        throw new Error("IHP Backend host url needs to start with \"http://\" or \"https://\", you passed \"" + host + "\"");
    }
    if (host.endsWith('/')) {
        throw new Error('IHP Backend host url should not have a trailing slash, please remove the last "/" from "' + host + '"');
    }
    DataSyncController.ihpBackendHost = host;
}

export async function createRecord<T extends TableName>(table: T, record: NewRecord<T>, options: CrudOptions = {}): Promise<IHPRecord<T>> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to createRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }
    if (record !== Object(record)) {
        throw new Error(`Record needs to be an object, you passed ${JSON.stringify(record)} in a call to createRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'CreateRecordMessage', table, record, transactionId };
    const shouldUpdateOptimistically = transactionId === null;

    try {
        if (shouldUpdateOptimistically) {
            createOptimisticRecord(table, record);
        }
        await waitPendingChanges(table, record);

        const response = await DataSyncController.getInstance().sendMessage(request);
        if (shouldUpdateOptimistically) {
            markCreateOptimisticRecordFinished(record);
        }
        return response.record as IHPRecord<T>;
    } catch (e) {
        if (shouldUpdateOptimistically) {
            undoCreateOptimisticRecord(table, record, e as Error);
        }

        throw new Error(`${(e as Error).message} while calling:\n\ncreateRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }
}

export async function updateRecord<T extends TableName>(table: T, id: UUID, patch: Partial<NewRecord<T>>, options: CrudOptions = {}): Promise<IHPRecord<T>> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to updateRecord(${JSON.stringify(table)}, ${JSON.stringify(id)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (typeof id !== "string") {
        throw new Error(`ID needs to be an UUID, you passed ${JSON.stringify(id)} in a call to updateRecord(${JSON.stringify(table)}, ${JSON.stringify(id)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (patch !== Object(patch)) {
        throw new Error(`Patch needs to be an object, you passed ${JSON.stringify(patch)} in a call to updateRecord(${JSON.stringify(table)}, ${JSON.stringify(id)}, ${JSON.stringify(patch, null, 4)})`);
    }

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'UpdateRecordMessage', table, id, patch, transactionId };

    const undoUpdateRecordOptimistic = transactionId === null
        ? updateRecordOptimistic(table, id, patch)
        : () => {};

    try {
        await waitPendingCreation(table, id);
        await waitPendingChanges(table, patch);
        const response = await DataSyncController.getInstance().sendMessage(request);

        return response.record as IHPRecord<T>;
    } catch (e) {
        undoUpdateRecordOptimistic();
        throw new Error((e as Error).message);
    }
}

export async function updateRecords<T extends TableName>(table: T, ids: UUID[], patch: Partial<NewRecord<T>>, options: CrudOptions = {}): Promise<IHPRecord<T>[]> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to updateRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (!Array.isArray(ids)) {
        throw new Error(`IDs need to be an array, you passed ${JSON.stringify(ids)} in a call to updateRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (patch !== Object(patch)) {
        throw new Error(`Patch needs to be an object, you passed ${JSON.stringify(patch)} in a call to updateRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)}, ${JSON.stringify(patch, null, 4)})`);
    }

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'UpdateRecordsMessage', table, ids, patch, transactionId };

    try {
        const response = await DataSyncController.getInstance().sendMessage(request);

        return response.records as IHPRecord<T>[];
    } catch (e) {
        throw new Error((e as Error).message);
    }
}

export async function deleteRecord<T extends TableName>(table: T, id: UUID, options: CrudOptions = {}): Promise<void> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to deleteRecord(${JSON.stringify(table)}, ${JSON.stringify(id)})`);
    }
    if (typeof id !== "string") {
        throw new Error(`ID needs to be an UUID, you passed ${JSON.stringify(id)} in a call to deleteRecord(${JSON.stringify(table)}, ${JSON.stringify(id)})`);
    }

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'DeleteRecordMessage', table, id, transactionId };

    const undoOptimisticDeleteRecord = transactionId === null
        ? deleteRecordOptimistic(table, id)
        : () => {};
    try {
        await waitPendingCreation(table, id);
        await DataSyncController.getInstance().sendMessage(request);

        return;
    } catch (e) {
        undoOptimisticDeleteRecord();
        throw new Error((e as Error).message);
    }
}

export async function deleteRecords<T extends TableName>(table: T, ids: UUID[], options: CrudOptions = {}): Promise<void> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to deleteRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)})`);
    }
    if (!Array.isArray(ids)) {
        throw new Error(`IDs needs to be an array, you passed ${JSON.stringify(ids)} in a call to deleteRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)})`);
    }

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'DeleteRecordsMessage', table, ids, transactionId };

    try {
        await DataSyncController.getInstance().sendMessage(request);

        return;
    } catch (e) {
        throw new Error((e as Error).message);
    }
}

export async function createRecords<T extends TableName>(table: T, records: NewRecord<T>[], options: CrudOptions = {}): Promise<IHPRecord<T>[]> {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to createRecords(${JSON.stringify(table)}, ${JSON.stringify(records, null, 4)})`);
    }
    if (!Array.isArray(records)) {
        throw new Error(`Records need to be an array, you passed ${JSON.stringify(records)} in a call to createRecords(${JSON.stringify(table)}, ${JSON.stringify(records, null, 4)})`);
    }

    const transactionId = options.transactionId ?? null;
    const request = { tag: 'CreateRecordsMessage', table, records, transactionId };

    try {
        const response = await DataSyncController.getInstance().sendMessage(request);

        return response.records as IHPRecord<T>[];
    } catch (e) {
        throw new Error((e as Error).message);
    }
}

function createOptimisticRecord<T extends TableName>(table: T, record: NewRecord<T>): void {
    const dataSyncController = DataSyncController.getInstance();

    // Ensure that the record has an ID
    if (record.id == null) {
        record.id = randomUUID();
    }
    registerPendingOptimisticCreate(record.id);

    // Optimistically set createdAt if the table has this field (dynamic check)
    const rec = record as Record<string, unknown>;
    if (dataSyncController.optimisticCreatedNeedsCreatedAtField.has(table) && rec.createdAt == null) {
        rec.createdAt = new Date();
    }

    for (const dataSubscription of dataSyncController.dataSubscriptions) {
        if (dataSubscription.query.table !== table) {
            continue;
        }
        if (!recordMatchesQuery(dataSubscription.query, rec as DataRecord)) {
            continue;
        }

        dataSubscription.onCreateOptimistic(rec as DataRecord);
    }

    dataSyncController.optimisticCreatedPendingRecordIds.push(record.id!);
}

function registerPendingOptimisticCreate(id: UUID): void {
    const dataSyncController = DataSyncController.getInstance();
    if (dataSyncController.pendingOptimisticCreates.has(id)) {
        return;
    }

    let resolve!: () => void;
    let reject!: (reason: Error) => void;
    const promise = new Promise<void>((resolvePromise, rejectPromise) => {
        resolve = resolvePromise;
        reject = rejectPromise;
    });
    // A create often has no dependent operation. Mark the rejection as handled
    // while still keeping the original promise rejectable for actual dependants.
    void promise.catch(() => {});
    dataSyncController.pendingOptimisticCreates.set(id, { promise, resolve, reject });
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

function undoCreateOptimisticRecord<T extends TableName>(table: T, record: NewRecord<T>, reason: Error): void {
    const dataSyncController = DataSyncController.getInstance();
    for (const dataSubscription of dataSyncController.dataSubscriptions) {
        if (dataSubscription.query.table !== table) {
            continue;
        }
        if (!dataSubscription.optimisticCreatedPendingRecordIds.includes(record.id!)) {
            continue;
        }

        dataSubscription.onDelete(record.id!);
    }

    markCreateOptimisticRecordFinished(record, reason);
}

function markCreateOptimisticRecordFinished<T extends TableName>(record: NewRecord<T>, error: Error | null = null): void {
    const dataSyncController = DataSyncController.getInstance();
    const index = dataSyncController.optimisticCreatedPendingRecordIds.indexOf(record.id!);
    if (index !== -1) {
        dataSyncController.optimisticCreatedPendingRecordIds.splice(index, 1);
    }

    const pendingCreate = dataSyncController.pendingOptimisticCreates.get(record.id!);
    if (pendingCreate) {
        dataSyncController.pendingOptimisticCreates.delete(record.id!);
        if (error) {
            pendingCreate.reject(error);
        } else {
            pendingCreate.resolve();
        }
    }
}

function updateRecordOptimistic<T extends TableName>(table: T, id: UUID, patch: Partial<NewRecord<T>>): () => void {
    const dataSyncController = DataSyncController.getInstance();
    const patchRecord = patch as Record<string, unknown>;
    const rollbackOperations: (() => void)[] = [];
    for (const dataSubscription of dataSyncController.dataSubscriptions) {
        if (dataSubscription.query.table !== table) {
            continue;
        }

        const dataSubscriptionRecords = dataSubscription.getRecords();
        if (!dataSubscriptionRecords) {
            continue;
        }

        for (const record of dataSubscriptionRecords) {
            if (!record || record.id !== id) {
                continue;
            }

            // Store values before we apply the patch to the record
            const oldValues: Record<string, unknown> = {};
            for (const key of Object.keys(patchRecord)) {
                oldValues[key] = record[key];
            }

            // Apply the patch optimistically
            dataSubscription.onUpdate(id, patchRecord, null);
            dataSubscription.optimisticUpdatedPendingRecordIds.add(id);

            rollbackOperations.push(() => {
                dataSubscription.optimisticUpdatedPendingRecordIds.delete(id);

                const records = dataSubscription.getRecords();
                if (!records) {
                    return;
                }

                const currentRecord = records.find(record => record.id === id);
                if (!currentRecord) {
                    return;
                }

                const undoPatch: Record<string, unknown> = {};
                for (const key of Object.keys(patchRecord)) {
                    if (currentRecord[key] === patchRecord[key]) {
                        undoPatch[key] = oldValues[key];
                    }
                }

                dataSubscription.onUpdate(id, undoPatch, null);
            });
        }
    }

    return () => {
        for (const rollbackOperation of rollbackOperations) {
            rollbackOperation();
        }
    };
}

function deleteRecordOptimistic<T extends TableName>(table: T, id: UUID): () => void {
    const dataSyncController = DataSyncController.getInstance();
    const undoOperations: (() => void)[] = [];
    for (const dataSubscription of dataSyncController.dataSubscriptions) {
        if (dataSubscription.query.table !== table) {
            continue;
        }

        const deletedRecord = dataSubscription.records!.find(record => record.id === id);
        if (deletedRecord) {
            dataSubscription.onDelete(id);
            undoOperations.push(() => dataSubscription.onCreate(deletedRecord));
        }
    }

    return () => {
        for (const undoOperation of undoOperations) {
            undoOperation();
        }
    };
}

function pendingOptimisticCreatesReferencedBy<T extends TableName>(record: NewRecord<T> | Partial<NewRecord<T>>): Promise<void>[] {
    const dataSyncController = DataSyncController.getInstance();
    const rec = record as Record<string, unknown>;
    const pendingCreates = new Set<Promise<void>>();

    for (const attribute in rec) {
        if (attribute === 'id') {
            continue; // The current record's id is always optimistic
        }
        const pendingCreate = dataSyncController.pendingOptimisticCreates.get(rec[attribute] as UUID);
        if (pendingCreate) {
            pendingCreates.add(pendingCreate.promise);
        }
    }

    return Array.from(pendingCreates);
}

async function waitPendingChanges<T extends TableName>(_table: T, record: NewRecord<T> | Partial<NewRecord<T>>): Promise<void> {
    await Promise.all(pendingOptimisticCreatesReferencedBy(record));
}

async function waitPendingCreation<T extends TableName>(_table: T, id: UUID): Promise<void> {
    const pendingCreate = DataSyncController.getInstance().pendingOptimisticCreates.get(id);
    if (pendingCreate) {
        await pendingCreate.promise;
    }
}

export { DataSyncController, DataSubscription, initIHPBackend, NewRecordBehaviour };
