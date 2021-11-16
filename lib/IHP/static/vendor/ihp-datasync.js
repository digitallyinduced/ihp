// TODO: reconnect websocket if connection dies
class DataSyncController {
    static getInstance() {
        if (!window.dataSyncController) {
            window.dataSyncController = new DataSyncController();
        }

        return window.dataSyncController;
    }

    static getWSUrl() {
        if (window.ihpBackendHost) {
            const host = window.ihpBackendHost
                .replace('https://', 'wss://')
                .replace('http://', 'ws://')
            return host + '/DataSyncController' + (ihpAuthUsingToken() ? '?access_token=' + encodeURIComponent(ihpAuthJWT()) : '');
        }

        var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
        var socketHost = socketProtocol + "://" + window.location.hostname + ":" + document.location.port + '/DataSyncController';
        return socketHost;
    }

    constructor() {
        this.requests = [];
        this.connection = null;
        this.requestIdCounter = 0;
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

        const promise = new Promise((resolve, reject) => {
            const socket = new WebSocket(DataSyncController.getWSUrl())

            socket.onopen = () => resolve(socket);
            socket.onclose = this.onClose.bind(this);
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

            if (payload.tag === 'DataSyncError') {
                reject(new Error(payload.errorMessage));
            } else {
                resolve(payload);
            }
        } else {
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

class DataSubscription {
    constructor(query) {
        this.query = query;
        this.createOnServer();
        this.isClosed = false;
        this.subscriptionId = null;
    }

    async createOnServer() {
        const dataSyncController = DataSyncController.getInstance();
        try {
            const { subscriptionId, result } = await dataSyncController.sendMessage({ tag: 'CreateDataSubscription', query: this.query });

            this.subscriptionId = subscriptionId;

            dataSyncController.addEventListener('message', message => {
                if (message.subscriptionId === this.subscriptionId) {
                    this.receiveUpdate(message);
                }
            });

            dataSyncController.addEventListener('close', this.onDataSyncClosed.bind(this));
            dataSyncController.addEventListener('reconnect', this.onDataSyncReconnect.bind(this));

            this.onReady(result);
        } catch (e) {
            throw new Error(e.message + ' while trying to sbuscribe to:\n' + JSON.stringify(this.query, null, 4));
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

        const dataSyncController = DataSyncController.getInstance();
        const { subscriptionId } = await dataSyncController.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId: this.subscriptionId });

        this.isClosed = true;
    }

    onDataSyncClosed() {
        this.isClosed = true;
    }

    async onDataSyncReconnect() {
        await this.createOnServer();
    }
}

function initIHPBackend({ host }) {
    window.ihpBackendHost = host;
}
