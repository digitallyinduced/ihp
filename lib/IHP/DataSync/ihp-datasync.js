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
        this.requests = [];
        this.connection = null;
        this.requestIdCounter = 0;
        this.receivedFirstResponse = false;
        this.eventListeners = {
            message: [],
            close: [],
            reconnect: []
        };
    }


    async getConnection() {
        if (this.connection) {
            return this.connection;
        }

        if (this.pendingConnection) {
            return await this.pendingConnection;
        }

        const connect = () => new Promise((resolve, reject) => {
            const socket = new WebSocket(DataSyncController.getWSUrl())

            socket.onopen = () => {
                // These handlers should only be installed once the connection is established
                socket.onclose = this.onClose.bind(this);
                socket.onmessage = this.onMessage.bind(this);

                resolve(socket);
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
        const request = this.requests.find(request => request.requestId === requestId);
        
        this.receivedFirstResponse = true;

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
            this.eventListeners.message.forEach(callback => callback(payload));
        }
    }

    onClose(event) {
        this.connection = null;

        for (const listener of this.eventListeners.close) {
            listener(event);
        }
        
        this.retryToReconnect();
    }

    async sendMessage(payload) {
        const connection = await this.getConnection();

        return new Promise((resolve, reject) => {
            payload.requestId = this.requestIdCounter++;
            connection.send(JSON.stringify(payload));
            this.requests.push({ requestId: payload.requestId, resolve, reject });
        });
    }

    addEventListener(event, callback) {
        this.eventListeners[event].push(callback);
    }

    retryToReconnect() {
        if (this.connection) {
            return;
        }

        setTimeout(async () => {
            console.log('Trying to reconnect DataSync ...');
            await this.getConnection();

            for (const listener of this.eventListeners.reconnect) {
                listener(event);
            }
        }, 2000);
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

        // When a new record is inserted, do we put it at the end or at the beginning?
        this.newRecordBehaviour = this.detectNewRecordBehaviour();
    }

    detectNewRecordBehaviour() {
        // If the query is ordered by the createdAt column, and the latest is at the top
        // we want to prepend new record
        const isOrderByCreatedAtDesc = this.query.orderByClause.length > 0 && this.query.orderByClause[0].orderByColumn === 'createdAt' && this.query.orderByClause[0].orderByColumn === 'createdAt';

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

            dataSyncController.addEventListener('message', message => {
                if (this.isClosed) {
                    return;
                }
                if (message.subscriptionId === this.subscriptionId) {
                    this.receiveUpdate(message);
                }
            });

            dataSyncController.addEventListener('close', this.onDataSyncClosed.bind(this));
            dataSyncController.addEventListener('reconnect', this.onDataSyncReconnect.bind(this));

            this.isConnected = true;
            this.records = result;

            this.resolveCreateOnServer(result);
            this.updateSubscribers();
        } catch (e) {
            this.connectError = new Error(e.message + ' while trying to subscribe to:\n' + JSON.stringify(this.query, null, 4));
            this.rejectCreateOnServer(this.connectError);
            throw this.connectError;
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
        this.records = shouldAppend ? [...this.records, newRecord] : [newRecord, ...this.records];
        this.updateSubscribers();
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

export async function createRecord(table, record) {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to createRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }
    if (record !== Object(record)) {
        throw new Error(`Record needs to be an object, you passed ${JSON.stringify(record)} in a call to createRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }

    const request = { tag: 'CreateRecordMessage', table, record };
    
    try {
        const response = await DataSyncController.getInstance().sendMessage(request);
        return response.record;
    } catch (e) {
        // We rethrow the error here for improved callstacks
        // Without this, the error location in the callstack would show the error to be caused
        // somewhere in DataSyncController. But the user is not really using DataSyncController
        // directly.
        throw new Error(`${e.message} while calling:\n\ncreateRecord(${JSON.stringify(table)}, ${JSON.stringify(record, null, 4)})`);
    }
}

export async function updateRecord(table, id, patch) {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to updateRecord(${JSON.stringify(table)}, ${JSON.stringify(id)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (typeof id !== "string") {
        throw new Error(`ID needs to be an UUID, you passed ${JSON.stringify(id)} in a call to updateRecord(${JSON.stringify(table)}, ${JSON.stringify(id)}, ${JSON.stringify(patch, null, 4)})`);
    }
    if (patch !== Object(patch)) {
        throw new Error(`Patch needs to be an object, you passed ${JSON.stringify(patch)} in a call to updateRecord(${JSON.stringify(table)}, ${JSON.stringify(id)}, ${JSON.stringify(patch, null, 4)})`);
    }

    const request = { tag: 'UpdateRecordMessage', table, id, patch };

    try {
        const response = await DataSyncController.getInstance().sendMessage(request);

        return response.record;
    } catch (e) {
        throw new Error(e.message);
    }
}

export async function deleteRecord(table, id) {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to deleteRecord(${JSON.stringify(table)}, ${JSON.stringify(id)})`);
    }
    if (typeof id !== "string") {
        throw new Error(`ID needs to be an UUID, you passed ${JSON.stringify(id)} in a call to deleteRecord(${JSON.stringify(table)}, ${JSON.stringify(id)})`);
    }

    const request = { tag: 'DeleteRecordMessage', table, id };

    try {
        const response = await DataSyncController.getInstance().sendMessage(request);

        return;
    } catch (e) {
        throw new Error(e.message);
    }
}

export async function createRecords(table, records) {
    if (typeof table !== "string") {
        throw new Error(`Table name needs to be a string, you passed ${JSON.stringify(table)} in a call to createRecords(${JSON.stringify(table)}, ${JSON.stringify(records, null, 4)})`);
    }
    if (!Array.isArray(records)) {
        throw new Error(`Records need to be an array, you passed ${JSON.stringify(records)} in a call to createRecords(${JSON.stringify(table)}, ${JSON.stringify(records, null, 4)})`);
    }
    
    const request = { tag: 'CreateRecordsMessage', table, records };
    
    try {
        const response = await DataSyncController.getInstance().sendMessage(request);

        return response.records;
    } catch (e) {
        throw new Error(e.message);
    }
}

export { DataSyncController, DataSubscription, initIHPBackend };