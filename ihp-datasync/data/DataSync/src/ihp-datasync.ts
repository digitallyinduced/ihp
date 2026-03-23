import { recordMatchesQuery } from './ihp-querybuilder.js';
import type {
    DynamicSQLQuery,
    DataRecord,
    UUID,
    TableName,
    IHPRecord,
    NewRecord,
    DataSyncEventType,
    DataSyncEventCallback,
    PendingRequest,
    ServerMessage,
    DataSubscriptionOptions,
    CrudOptions,
} from './types.js';
import { APPEND_NEW_RECORD, PREPEND_NEW_RECORD, NewRecordBehaviour } from './types.js';

type EventListeners = {
    [K in DataSyncEventType]: DataSyncEventCallback[];
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
    outbox: string[];
    reconnectTimeout: ReturnType<typeof setTimeout> | null;
    pendingRequestTimeout: ReturnType<typeof setTimeout> | null;
    dataSubscriptions: DataSubscription[];
    optimisticCreatedPendingRecordIds: UUID[];
    optimisticCreatedNeedsCreatedAtField: Set<string>;
    messageTimeout: number;
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
        this.pendingRequestTimeout = null;
        this.dataSubscriptions = [];
        this.optimisticCreatedPendingRecordIds = [];
        this.optimisticCreatedNeedsCreatedAtField = new Set();
        this.messageTimeout = 5000;
        this.pendingConnection = null;
    }

    async startConnection(): Promise<WebSocket> {
        if (this.connection) {
            return this.connection;
        }

        if (this.pendingConnection) {
            return await this.pendingConnection;
        }

        const connect = (): Promise<WebSocket> => new Promise((resolve, reject) => {
            const socket = new WebSocket(DataSyncController.getWSUrl());

            socket.onopen = (event) => {
                socket.onclose = this.onClose.bind(this);
                socket.onmessage = this.onMessage.bind(this);

                resolve(socket);

                for (const listener of this.eventListeners.open) {
                    listener(event);
                }
            };

            socket.onerror = (event) => reject(event);
        });
        const wait = (timeout: number): Promise<void> => new Promise((resolve) => setTimeout(resolve, timeout));

        const MAX_RETRIES = 32;
        const MAX_DELAY_EXPONENT = 6; // 2 ^ 6 ~> 1 min

        for (let i = 0; i < MAX_RETRIES; i++) {
            this.pendingConnection = connect();

            try {
                const socket = await this.pendingConnection;
                this.pendingConnection = null;
                this.connection = socket;

                // Flush outbox
                for (let j = 0; j < this.outbox.length; j++) {
                    this.connection.send(this.outbox[j]);
                }

                this.outbox = [];

                return this.connection;
            } catch (error) {
                const time = Math.pow(2, Math.min(i, MAX_DELAY_EXPONENT)); // 2, 4, 8, 16, 32, ...
                console.log('Retrying in ', time, 'secs');
                await wait(time * 1000);
            }
        }

        throw new Error('Unable to connect to the DataSync Websocket');
    }

    onMessage(event: MessageEvent): void {
        const payload: ServerMessage = JSON.parse(event.data as string);
        const requestId = payload.requestId;
        const request = this.pendingRequests.find(request => request.requestId === requestId);

        if (request !== undefined) {
            this.pendingRequests.splice(this.pendingRequests.indexOf(request), 1);
        }

        this.receivedFirstResponse = true;

        if (this.pendingRequestTimeout) {
            clearTimeout(this.pendingRequestTimeout);
            this.pendingRequestTimeout = null;
        }

        if (request) {
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

        this.eventListeners.message.slice(0).forEach(callback => callback(payload));
    }

    onClose(_event: CloseEvent | null): void {
        this.connection = null;

        for (const listener of this.eventListeners.close) {
            listener(_event);
        }

        this.retryToReconnect();
    }

    async sendMessage(payload: Record<string, unknown>): Promise<ServerMessage> {
        return new Promise((resolve, reject) => {
            payload.requestId = this.requestIdCounter++;
            this.pendingRequests.push({ requestId: payload.requestId as number, resolve: resolve as (value: unknown) => void, reject });

            if (this.connection === null) {
                this.outbox.push(JSON.stringify(payload));

                const isFirstMessage = this.requestIdCounter === 1;
                if (isFirstMessage) {
                    this.startConnection();
                }
            } else {
                this.connection.send(JSON.stringify(payload));

                if (!this.pendingRequestTimeout) {
                    this.pendingRequestTimeout = setTimeout(this.onPendingRequestTimeout.bind(this), this.messageTimeout);
                }
            }
        });
    }

    addEventListener(event: DataSyncEventType, callback: DataSyncEventCallback): void {
        this.eventListeners[event].push(callback);
    }

    removeEventListener(event: DataSyncEventType, callback: DataSyncEventCallback): void {
        const index = this.eventListeners[event].indexOf(callback);
        if (index > -1) {
            this.eventListeners[event].splice(index, 1);
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
            try {
                console.log('Trying to reconnect DataSync ...');
                await this.startConnection();

                for (const listener of this.eventListeners.reconnect) {
                    listener(undefined);
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

    onPendingRequestTimeout(): void {
        if (this.connection) {
            console.log('Pending request timed out, closing WebSocket');
            this.connection.close();
            this.onClose(null);
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

    constructor(query: DynamicSQLQuery, options: DataSubscriptionOptions | null = null, cache: Map<string, DataRecord[]> | null = null) {
        if (typeof query !== "object" || !('table' in query)) {
            throw new Error("Query passed to `new DataSubscription(..)` doesn't look like a query object. If you're using the `query()` functions to costruct the object, make sure you pass the `.query` property, like this: `new DataSubscription(query('my_table').orderBy('createdAt').query)`");
        }
        this.query = query;
        this.createOnServerPromise = new Promise((resolve, reject) => {
            this.resolveCreateOnServer = resolve;
            this.rejectCreateOnServer = reject;
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
                dataSyncController.addEventListener('message', this.onMessage as DataSyncEventCallback);
                dataSyncController.addEventListener('close', this.onDataSyncClosed as DataSyncEventCallback);
                dataSyncController.addEventListener('reconnect', this.onDataSyncReconnect as DataSyncEventCallback);
                dataSyncController.dataSubscriptions.push(this);
            }

            this.isConnected = true;
            this.isClosed = false;
            this.records = result;

            this.resolveCreateOnServer(result);
            this.updateSubscribers();

            dataSyncController.learnOptimisticShapeFromResult(this.query.table, result);
        } catch (e) {
            const error = e as Error;
            this.connectError = new Error(error.message + ' while trying to subscribe to:\n' + JSON.stringify(this.query, null, 4));
            this.rejectCreateOnServer(this.connectError);
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
        if (this.isClosed) {
            return;
        }

        // We cannot close the DataSubscription when the subscriptionId is not assigned
        if (!this.isClosed && !this.isConnected) {
            await this.createOnServerPromise;
            return this.close();
        }

        // Set isClosed early as we need to prevent a second close() from triggering another DeleteDataSubscription message
        // also we don't want to receive any further messages, and onMessage will not process if isClosed == true
        this.isClosed = true;
        this.onClose();

        const dataSyncController = DataSyncController.getInstance();
        await dataSyncController.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId: this.subscriptionId });

        dataSyncController.removeEventListener('message', this.onMessage as DataSyncEventCallback);
        dataSyncController.removeEventListener('close', this.onDataSyncClosed as DataSyncEventCallback);
        dataSyncController.removeEventListener('reconnect', this.onDataSyncReconnect as DataSyncEventCallback);
        const index = dataSyncController.dataSubscriptions.indexOf(this);
        if (index !== -1) {
            dataSyncController.dataSubscriptions.splice(index, 1);
        }

        this.isConnected = false;
    }

    onDataSyncClosed(): void {
        this.isClosed = true;
        this.isConnected = false;
    }

    async onDataSyncReconnect(): Promise<void> {
        await this.createOnServer();
    }

    onUpdate(id: UUID, changeSet: Record<string, unknown> | null, appendSet: Record<string, unknown> | null): void {
        this.records = this.records!.map(record => {
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
        });

        this.optimisticUpdatedPendingRecordIds.delete(id);
        this.updateSubscribers();
    }

    onCreate(newRecord: DataRecord): void {
        const shouldAppend = this.newRecordBehaviour === APPEND_NEW_RECORD;

        const newRecordId = newRecord.id;
        const isOptimisticallyCreatedAlready = this.optimisticCreatedPendingRecordIds.indexOf(newRecordId) !== -1;
        if (isOptimisticallyCreatedAlready) {
            this.onUpdate(newRecordId, newRecord, null);
            this.optimisticCreatedPendingRecordIds.splice(this.optimisticCreatedPendingRecordIds.indexOf(newRecordId), 1);
        } else {
            this.records = shouldAppend ? [...this.records!, newRecord] : [newRecord, ...this.records!];
        }

        this.updateSubscribers();
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
        this.updateSubscribers();
    }

    subscribe(callback: (records: DataRecord[] | null) => void): () => void {
        this.subscribers.push(callback);

        return () => {
            const index = this.subscribers.indexOf(callback);
            if (index !== -1) {
                this.subscribers.splice(index, 1);
            }

            // We delay the close as react could be re-rendering a component
            // we garbage collect this connecetion once it's clearly not used anymore
            setTimeout(this.closeIfNotUsed.bind(this), 1000);
        };
    }

    updateSubscribers(): void {
        if (this.cache) {
            this.cache.set(JSON.stringify(this.query), this.records!);
        }
        for (const subscriber of this.subscribers) {
            subscriber(this.records);
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

    const transactionId = 'transactionId' in options ? options.transactionId : null;
    const request = { tag: 'CreateRecordMessage', table, record, transactionId };

    try {
        createOptimisticRecord(table, record);
        await waitPendingChanges(table, record);

        const response = await DataSyncController.getInstance().sendMessage(request);
        markCreateOptimisticRecordFinished(record);
        return response.record as IHPRecord<T>;
    } catch (e) {
        undoCreateOptimisticRecord(table, record);

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

    const transactionId = 'transactionId' in options ? options.transactionId : null;
    const request = { tag: 'UpdateRecordMessage', table, id, patch, transactionId };

    const undoUpdateRecordOptimistic = updateRecordOptimistic(table, id, patch);

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

    const transactionId = 'transactionId' in options ? options.transactionId : null;
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

    const transactionId = 'transactionId' in options ? options.transactionId : null;
    const request = { tag: 'DeleteRecordMessage', table, id, transactionId };

    const undoOptimisticDeleteRecord = deleteRecordOptimistic(table, id);
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

    const transactionId = 'transactionId' in options ? options.transactionId : null;
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

    const transactionId = 'transactionId' in options ? options.transactionId : null;
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
    // NewRecord<T> always has { id?: UUID, ... } shape at runtime
    const rec = record as NewRecord<T> & { id?: UUID; createdAt?: unknown };

    // Ensure that the record has an ID
    if (rec.id === null || rec.id === undefined) {
        rec.id = randomUUID();
    }

    if (dataSyncController.optimisticCreatedNeedsCreatedAtField.has(table) && (rec.createdAt === null || rec.createdAt === undefined)) {
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

    dataSyncController.optimisticCreatedPendingRecordIds.push(rec.id);
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

function undoCreateOptimisticRecord<T extends TableName>(table: T, record: NewRecord<T>): void {
    const dataSyncController = DataSyncController.getInstance();
    const rec = record as NewRecord<T> & { id: UUID };
    for (const dataSubscription of dataSyncController.dataSubscriptions) {
        if (dataSubscription.query.table !== table) {
            continue;
        }

        dataSubscription.onDelete(rec.id);
    }

    markCreateOptimisticRecordFinished(record);
}

function markCreateOptimisticRecordFinished<T extends TableName>(record: NewRecord<T>): void {
    const dataSyncController = DataSyncController.getInstance();
    const rec = record as NewRecord<T> & { id: UUID };
    const index = dataSyncController.optimisticCreatedPendingRecordIds.indexOf(rec.id);
    if (index !== -1) {
        dataSyncController.optimisticCreatedPendingRecordIds.splice(index, 1);
    }
}

function updateRecordOptimistic<T extends TableName>(table: T, id: UUID, patch: Partial<NewRecord<T>>): () => void {
    const dataSyncController = DataSyncController.getInstance();
    const patchRecord = patch as DataRecord;
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

function doesRecordReferencePendingOptimisticRecord<T extends TableName>(record: NewRecord<T> | Partial<NewRecord<T>>): boolean {
    const dataSyncController = DataSyncController.getInstance();
    const optimisticIds = dataSyncController.optimisticCreatedPendingRecordIds;
    const rec = record as DataRecord;

    for (const attribute in rec) {
        if (attribute === 'id') {
            continue; // The current record's id is always optimistic
        }
        if (optimisticIds.indexOf(rec[attribute] as UUID) !== -1) {
            return true;
        }
    }

    return false;
}

async function waitPendingChanges<T extends TableName>(_table: T, record: NewRecord<T> | Partial<NewRecord<T>>): Promise<void> {
    if (doesRecordReferencePendingOptimisticRecord(record)) {
        const rec = record as NewRecord<T> & { id: UUID };
        return waitForMessageMatching(message => message.tag === 'DidCreateRecord' && (message as ServerMessage).record != null && ((message as ServerMessage).record as DataRecord).id === rec.id);
    }
}

async function waitPendingCreation<T extends TableName>(_table: T, id: UUID): Promise<void> {
    const optimisticIds = DataSyncController.getInstance().optimisticCreatedPendingRecordIds;
    if (optimisticIds.indexOf(id) !== -1) {
        return waitForMessageMatching(message => message.tag === 'DidCreateRecord' && (message as ServerMessage).record != null && ((message as ServerMessage).record as DataRecord).id === id);
    }
}

function waitForMessageMatching(condition: (message: ServerMessage) => boolean): Promise<void> {
    const dataSyncController = DataSyncController.getInstance();

    return new Promise((resolve) => {
        const callback: DataSyncEventCallback = (payload) => {
            if (condition(payload as ServerMessage)) {
                dataSyncController.removeEventListener('message', callback);
                resolve();
            }
        };

        dataSyncController.addEventListener('message', callback);
    });
}

export { DataSyncController, DataSubscription, initIHPBackend, NewRecordBehaviour };
