function getWebSocketHost(app) {
    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    var socketHost = socketProtocol + "://" + window.location.hostname + ":" + document.location.port + '/' + app;
    return socketHost;
}

// TODO: reconnect websocket if connection dies
class DataSyncController {
    static getInstance() {
        if (!window.dataSyncController) {
            window.dataSyncController = new DataSyncController();
        }

        return window.dataSyncController;
    }

    constructor() {
        this.requests = [];
        this.connection = null;
        this.requestIdCounter = 0;
        this.eventListeners = {
            message: []
        };
    }


    async getConnection() {
        if (this.connection) {
            return this.connection;
        }

        if (this.pendingConnection) {
            return await this.pendingConnection;
        }

        const promise = new Promise((resolve, reject) => {
            const socket = new WebSocket(getWebSocketHost('DataSyncController'))

            socket.onopen = () => resolve(socket);
            socket.onmessage = this.onMessage.bind(this);
        });

        this.pendingConnection = promise;

        const socket = await promise;

        this.pendingConnection = null;
        this.connection = socket;

        return socket;
    }

    onMessage(event) {
        const payload = JSON.parse(event.data);

        const requestId = payload.requestId;

        const request = this.requests.find(request => request.requestId === requestId);
        if (request) {
            const { resolve, reject } = request;
            resolve(payload);
        } else {
            this.eventListeners.message.forEach(callback => callback(payload));
        }
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
}

class DataSubscription {
    constructor(query) {
        this.query = query;
        this.createOnServer();
        this.isClosed = false;
        this.subscriptionId = null;
    }

    async createOnServer() {
        const dataSyncController = DataSyncController.getInstance();
        const { subscriptionId, result } = await dataSyncController.sendMessage({ tag: 'CreateDataSubscription', query: this.query });

        this.subscriptionId = subscriptionId;

        dataSyncController.addEventListener('message', message => {
            if (message.subscriptionId === this.subscriptionId) {
                this.receiveUpdate(message);
            }
        });

        this.onReady(result);
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

        const dataSyncController = DataSyncController.getInstance();
        const { subscriptionId } = await dataSyncController.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId: this.subscriptionId });

        this.isClosed = true;
    }
}