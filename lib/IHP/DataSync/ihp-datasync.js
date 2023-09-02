import { recordMatchesQuery } from './ihp-querybuilder.js';

class DataSyncController {
    static instance = null;
    static ihpBackendHost = null;

    static getInstance() {
        if (!DataSyncController.instance) {
            DataSyncController.instance = new DataSyncController();
        }

        return DataSyncController.instance;
    }

    static getWSUrl() {
        if (DataSyncController.ihpBackendHost) {
            const jwt = localStorage.getItem('ihp_jwt');
            const host = DataSyncController.ihpBackendHost
                .replace('https://', 'wss://')
                .replace('http://', 'ws://')
            return host + '/DataSyncController' + (jwt !== null ? '?access_token=' + encodeURIComponent(jwt) : '');
        }

        var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
        var socketHost = socketProtocol + "://" + document.location.hostname + ":" + document.location.port + '/DataSyncController';
        return socketHost;
    }

    constructor() {
        this.pendingRequests = []; // Stores the pending requests, contains objects of shape { requestId, resolve, reject }
        this.connection = null;
        this.requestIdCounter = 0;
        this.receivedFirstResponse = false;
        this.eventListeners = {
            message: [],
            close: [],
            reconnect: [],
            open: []
        };

        // We store messages here that cannot be send because the connection is not available
        this.outbox = [];
        this.reconnectTimeout = null;
    
        // Every message from the client expects a reply from the server
        // If that doesn't arrive in time, we can expect the connection to be broken
        this.pendingRequestTimeout = null;

        this.dataSubscriptions = [];

        // Ids of records that are visible in the UI already, but yet still have to be confirmed by the server
        this.optimisticCreatedPendingRecordIds = [];

        // When doing optimistic inserts, we want to optimistically set the `createdAt` field, if the record will have such a field after insert
        // For that we need to keep track of all tables that have a `createdAt` field.
        this.optimisticCreatedNeedsCreatedAtField = new Set();
    }

    async startConnection() {
        if (this.connection) {
            return this.connection;
        }

        if (this.pendingConnection) {
            return await this.pendingConnection;
        }

        const connect = () => new Promise((resolve, reject) => {
            const socket = new WebSocket(DataSyncController.getWSUrl())

            socket.onopen = event => {
                // These handlers should only be installed once the connection is established
                socket.onclose = this.onClose.bind(this);
                socket.onmessage = this.onMessage.bind(this);

                resolve(socket);

                for (const listener of this.eventListeners.open) {
                    listener(event);
                }
            }

            socket.onerror = (event) => reject(event);
        });
        const wait = timeout => new Promise((resolve) => setTimeout(resolve, timeout));

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

    onMessage(event) {
        const payload = JSON.parse(event.data);
        const requestId = payload.requestId;
        const request = this.pendingRequests.find(request => request.requestId === requestId);

        // Remove request from array, as we don't need it anymore. If we don't remove it we will
        // build up a lot of memory and slow down the app over time
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
                reject(new Error(payload.errorMessage));
            } else {
                resolve(payload);
            }
        } else {
            if (payload.tag === 'FailedToDecodeMessageError') {
                throw new Error(payload.errorMessage);
            }
        }

        this.eventListeners.message.slice(0).forEach(callback => callback(payload));
    }

    onClose(event) {
        this.connection = null;

        for (const listener of this.eventListeners.close) {
            listener(event);
        }
        
        this.retryToReconnect();
    }

    async sendMessage(payload) {
        return new Promise((resolve, reject) => {
            payload.requestId = this.requestIdCounter++;
            this.pendingRequests.push({ requestId: payload.requestId, resolve, reject });

            if (this.connection === null) {
                this.outbox.push(JSON.stringify(payload));

                let isFirstMessage = this.requestIdCounter === 1;
                if (isFirstMessage) {
                    this.startConnection();
                }
            } else {
                this.connection.send(JSON.stringify(payload));
                
                if (!this.pendingRequestTimeout) {
                    this.pendingRequestTimeout = setTimeout(this.onPendingRequestTimeout.bind(this), 5000);
                }
            }
        });
    }

    addEventListener(event, callback) {
        this.eventListeners[event].push(callback);
    }

    removeEventListener(event, callback) {
        const index = this.eventListeners[event].indexOf(callback);
        if (index > -1) {
            this.eventListeners[event].splice(index, 1);
        }
    }

    retryToReconnect() {
        if (this.connection) {
            return;
        }

        if (this.reconnectTimeout) {
            clearTimeout(this.reconnectTimeout);
        }
        this.reconnectTimeout = setTimeout(async () => {
            console.log('Trying to reconnect DataSync ...');
            await this.startConnection();

            for (const listener of this.eventListeners.reconnect) {
                listener();
            }
        }, 1000);
    }


    learnOptimisticShapeFromResult(table, result) {
        if (result.length > 0) {
            const hasCreatedAtField = 'createdAt' in result[0];
            if (hasCreatedAtField) {
                this.optimisticCreatedNeedsCreatedAtField.add(table);
            }
        }
    }

    onPendingRequestTimeout() {
        if (this.connection) {
            console.log('Pending request timed out, closing WebSocket');
            this.connection.close();
            this.onClose(null);
        }
    }
}

const APPEND_NEW_RECORD = 0;
const PREPEND_NEW_RECORD = 1;

class DataSubscription {
    constructor(query) {
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
        this.records = [];

        this.getRecords = this.getRecords.bind(this);
        this.subscribe = this.subscribe.bind(this);
        this.onDataSyncClosed = this.onDataSyncClosed.bind(this);
        this.onDataSyncReconnect = this.onDataSyncReconnect.bind(this);
        this.onMessage = this.onMessage.bind(this);

        // When a new record is inserted, do we put it at the end or at the beginning?
        this.newRecordBehaviour = this.detectNewRecordBehaviour();
        
        this.optimisticCreatedPendingRecordIds = [];
    }

    detectNewRecordBehaviour() {
        // If the query is ordered by the createdAt column, and the latest is at the top
        // we want to prepend new record
        const isOrderByCreatedAtDesc = this.query.orderByClause.length > 0 && this.query.orderByClause[0].orderByColumn === 'createdAt' && this.query.orderByClause[0].orderByDirection === 'Desc';

        if (isOrderByCreatedAtDesc) {
            return PREPEND_NEW_RECORD;
        }

        return APPEND_NEW_RECORD;
    }

    async createOnServer() {
        const dataSyncController = DataSyncController.getInstance();
        try {
            const { subscriptionId, result } = await dataSyncController.sendMessage({ tag: 'CreateDataSubscription', query: this.query });

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
            this.records = result;

            this.resolveCreateOnServer(result);
            this.updateSubscribers();
            
            dataSyncController.learnOptimisticShapeFromResult(this.query.table, result);
        } catch (e) {
            this.connectError = new Error(e.message + ' while trying to subscribe to:\n' + JSON.stringify(this.query, null, 4));
            this.rejectCreateOnServer(this.connectError);
            throw this.connectError;
        }
    }

    onMessage(message) {
        if (this.isClosed) {
            return;
        }
        if (message.subscriptionId === this.subscriptionId) {
            this.receiveUpdate(message);
        }
    }

    receiveUpdate(message) {
        const tag = message.tag;
        if (tag === 'DidUpdate') {
            this.onUpdate(message.id, message.changeSet);
        } else if (tag === 'DidInsert') {
            this.onCreate(message.record);
        } else if (tag === 'DidDelete') {
            this.onDelete(message.id);
        }
    }

    async close() {
        if (this.isClosed) {
            return;
        }

        // We cannot close the DataSubscription when the subscriptionId is not assigned
        if (!this.isClosed && !this.isConnected) {
            await this.createOnServerPromise;
            this.close();
            return;
        }

        const dataSyncController = DataSyncController.getInstance();
        const { subscriptionId } = await dataSyncController.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId: this.subscriptionId });
        
        dataSyncController.removeEventListener('message', this.onMessage);
        dataSyncController.removeEventListener('close', this.onDataSyncClosed);
        dataSyncController.removeEventListener('reconnect', this.onDataSyncReconnect);
        dataSyncController.dataSubscriptions.splice(dataSyncController.dataSubscriptions.indexOf(this), 1);

        this.isClosed = true;
        this.isConnected = false;

        this.onClose();
    }

    onDataSyncClosed() {
        this.isClosed = true;
        this.isConnected = false;
    }

    async onDataSyncReconnect() {
        await this.createOnServer();
    }

    onUpdate(id, changeSet) {
        this.records = this.records.map(record => {
            if (record.id === id) {
                return Object.assign({}, record, changeSet);
            }
            
            return record;
        });

        this.updateSubscribers();
    }

    onCreate(newRecord) {
        const shouldAppend = this.newRecordBehaviour === APPEND_NEW_RECORD;

        const isOptimisticallyCreatedAlready = this.optimisticCreatedPendingRecordIds.indexOf(newRecord.id) !== -1;
        if (isOptimisticallyCreatedAlready) {
            this.onUpdate(newRecord.id, newRecord);
            this.optimisticCreatedPendingRecordIds.slice(this.optimisticCreatedPendingRecordIds.indexOf(newRecord.id), 1);
        } else {
            this.records = shouldAppend ? [...this.records, newRecord] : [newRecord, ...this.records];
        }

        this.updateSubscribers();
    }

    onCreateOptimistic(newRecord) {
        if (!('id' in newRecord)) {
            throw new Error('Requires the record to have an id');
        }

        this.onCreate(newRecord);
        this.optimisticCreatedPendingRecordIds.push(newRecord.id);
    }

    onDelete(id) {
        this.records = this.records.filter(record => record.id !== id);
        this.updateSubscribers();
    }

    subscribe(callback) {
        if (this.isClosed) {
            throw new Error('Trying to subscribe to a DataSubscription that is already closed');
        }

        this.subscribers.push(callback);

        return () => {
            this.subscribers.splice(this.subscribers.indexOf(callback), 1);

            this.closeIfNotUsed();
        }
    }

    updateSubscribers() {
        for (const subscriber of this.subscribers) {
            subscriber(this.records);
        }
    }

    getRecords() {
        return this.records;
    }

    /**
     * If there's no subscriber on this DataSubscription, we will close it.
     */
    closeIfNotUsed() {
        const isUsed = this.subscribers.length > 0;
        if (isUsed) {
            return;
        }

        this.close();
    }

    onClose() {
        // Overriden by the react 18 integration to remove the closed connection from the DataSubscriptionStore
    }
}

function initIHPBackend({ host }) {
    if (typeof host !== "string" || (!host.startsWith("http://") && !host.startsWith("https://"))) {
        throw new Error("IHP Backend host url needs to start with \"http://\" or \"https://\", you passed \"" +  host + "\"")
    }
    if (host.endsWith('/')) {
        throw new Error('IHP Backend host url should not have a trailing slash, please remove the last \"/\" from \"' + host + "\"")
    }
    DataSyncController.ihpBackendHost = host;
}

export async function createRecord(table, record, options = {}) {
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
        return response.record;
    } catch (e) {
        undoCreateOptimisticRecord(table, record);

        // We rethrow the error here for improved callstacks
        // Without this, the error location in the callstack would show the error to be caused
        // somewhere in DataSyncController. But the user is not really using DataSyncController
        // directly.
        throw new Error(`${e.message} while calling:\n\ncreateRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }
}

export async function updateRecord(table, id, patch, options = {}) {
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

        return response.record;
    } catch (e) {
        undoUpdateRecordOptimistic();
        throw new Error(e.message);
    }
}

export async function updateRecords(table, ids, patch, options = {}) {
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

        return response.records;
    } catch (e) {
        throw new Error(e.message);
    }
}

export async function deleteRecord(table, id, options = {}) {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to deleteRecord(${JSON.stringify(table)}, ${JSON.stringify(id)})`);
    }
    if (typeof id !== "string") {
        throw new Error(`ID needs to be an UUID, you passed ${JSON.stringify(id)} in a call to deleteRecord(${JSON.stringify(table)}, ${JSON.stringify(id)})`);
    }

    const transactionId = 'transactionId' in options ? options.transactionId : null;
    const request = { tag: 'DeleteRecordMessage', table, id, transactionId };

    let undoOptimisticDeleteRecord = deleteRecordOptimistic(table, id);;
    try {
        await waitPendingCreation(table, id);
        const response = await DataSyncController.getInstance().sendMessage(request);

        return;
    } catch (e) {
        undoOptimisticDeleteRecord();
        throw new Error(e.message);
    }
}

export async function deleteRecords(table, ids, options = {}) {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to deleteRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)})`);
    }
    if (!Array.isArray(ids)) {
        throw new Error(`IDs needs to be an array, you passed ${JSON.stringify(ids)} in a call to deleteRecords(${JSON.stringify(table)}, ${JSON.stringify(ids)})`);
    }

    const transactionId = 'transactionId' in options ? options.transactionId : null;
    const request = { tag: 'DeleteRecordsMessage', table, ids, transactionId };

    try {
        const response = await DataSyncController.getInstance().sendMessage(request);

        return;
    } catch (e) {
        throw new Error(e.message);
    }
}

export async function createRecords(table, records, options = {}) {
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

        return response.records;
    } catch (e) {
        throw new Error(e.message);
    }
}

function createOptimisticRecord(table, record) {
    const dataSyncController = DataSyncController.getInstance();

    // Ensure that the record has an ID
    if (record.id === null || record.id === undefined) {
        record.id = randomUUID();
    }

    if (dataSyncController.optimisticCreatedNeedsCreatedAtField.has(table) && (record.createdAt === null || record.createdAt === undefined)) {
        record.createdAt = new Date();
    }

    for (const dataSubscription of dataSyncController.dataSubscriptions) {
        if (dataSubscription.query.table !== table) {
            continue;
        }
        if (!recordMatchesQuery(dataSubscription.query, record)) {
            continue;
        }

        dataSubscription.onCreateOptimistic(record);
    }

    dataSyncController.optimisticCreatedPendingRecordIds.push(record.id);
}

function randomUUID() {
    // Some older browsers like firefox 91 ESR don't support crypto.randomUUID
    // So we have a fallback to keep the app working in these browsers
    try {
        return crypto.randomUUID();
    } catch (e) {
        // https://stackoverflow.com/a/873856/14144232
        // http://www.ietf.org/rfc/rfc4122.txt
        var s = [];
        var hexDigits = "0123456789abcdef";
        for (var i = 0; i < 36; i++) {
            s[i] = hexDigits.substr(Math.floor(Math.random() * 0x10), 1);
        }
        s[14] = "4";  // bits 12-15 of the time_hi_and_version field to 0010
        // @ts-expect-error 
        s[19] = hexDigits.substr((s[19] & 0x3) | 0x8, 1);  // bits 6-7 of the clock_seq_hi_and_reserved to 01
        s[8] = s[13] = s[18] = s[23] = "-";

        var uuid = s.join("");
        return uuid;
    }
}

function undoCreateOptimisticRecord(table, record) {
    const dataSyncController = DataSyncController.getInstance();
    for (const dataSubscription of dataSyncController.dataSubscriptions) {
        if (dataSubscription.query.table !== table) {
            continue;
        }

        dataSubscription.onDelete(record.id);
    }

    markCreateOptimisticRecordFinished(record)
}

function markCreateOptimisticRecordFinished(record) {
    const dataSyncController = DataSyncController.getInstance();
    dataSyncController.optimisticCreatedPendingRecordIds.splice(dataSyncController.optimisticCreatedPendingRecordIds.indexOf(record.id), 1);
}

function updateRecordOptimistic(table, id, patch) {
    const dataSyncController = DataSyncController.getInstance();
    const rollbackOperations = [];
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
            const oldValues = {};
            for (const key of Object.keys(patch)) {
                oldValues[key] = record[key];
            }

            // Apply the patch optimistically
            dataSubscription.onUpdate(id, patch);

            rollbackOperations.push(() => {
                const records = dataSubscription.getRecords();
                if (!records) {
                    return;
                }

                const record = records.find(record => record.id === id)
                if (!record) {
                    return;
                }

                const undoPatch = {};
                for (const key of Object.keys(patch)) {
                    // There could be another update that has been applied after inbetween
                    // If the values are still the patched values, we roll it back. If it's different
                    // we asume it was changed by a different update operation.
                    if (record[key] === patch[key]) {
                        undoPatch[key] = oldValues[key];
                    }
                }

                dataSubscription.onUpdate(id, undoPatch);
            })
        }
    }

    return () => {
        for (const rollbackOperation of rollbackOperations) {
            rollbackOperation();
        }
    }
}

function deleteRecordOptimistic(table, id) {
    const dataSyncController = DataSyncController.getInstance();
    const undoOperations = [];
    for (const dataSubscription of dataSyncController.dataSubscriptions) {
        if (dataSubscription.query.table !== table) {
            continue;
        }

        const deletedRecord = dataSubscription.records.find(record => record.id === id);
        if (deletedRecord) {
            dataSubscription.onDelete(id);
            undoOperations.push(() => dataSubscription.onCreate(deletedRecord));
        }
    }

    return () => {
        for (const undoOperation of undoOperations) {
            undoOperation();
        }
    }
}

function doesRecordReferencePendingOptimisticRecord(record) {
    const dataSyncController = DataSyncController.getInstance();
    const optimisticIds = dataSyncController.optimisticCreatedPendingRecordIds;

    for (const attribute in record) {
        if (attribute === 'id') {
            continue; // The current record's id is always optimistic
        }
        if (optimisticIds.indexOf(record[attribute]) !== -1) {
            return true;
        }
    }

    return false;
}

async function waitPendingChanges(table, record) {
    if (doesRecordReferencePendingOptimisticRecord(record)) {
        return waitForMessageMatching(message => message.tag === 'DidCreateRecord' && message.record && message.record.id === record.id);
    }
}

async function waitPendingCreation(table, id) {
    const optimisticIds = DataSyncController.getInstance().optimisticCreatedPendingRecordIds;
    if (optimisticIds.indexOf(id) !== -1) {
        return waitForMessageMatching(message => message.tag === 'DidCreateRecord' && message.record && message.record.id === id);
    }
}

function waitForMessageMatching(condition) {
    const dataSyncController = DataSyncController.getInstance();
        
    return new Promise((resolve, reject) => {
        const callback = (message) => {
            if (condition(message)) {
                dataSyncController.removeEventListener('message', callback);        
                resolve();
            }
        }

        dataSyncController.addEventListener('message', callback);
    });
}

export { DataSyncController, DataSubscription, initIHPBackend };
