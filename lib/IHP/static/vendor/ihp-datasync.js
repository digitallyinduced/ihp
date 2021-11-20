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
        if (typeof query !== "object" || !('table' in query)) {
            throw new Error("Query passed to `new DataSubscription(..)` doesn't look like a query object. If you're using the `query()` functions to costruct the object, make sure you pass the `.query` property, like this: `new DataSubscription(query('my_table').orderBy('createdAt').query)`");
        }
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
            throw new Error(e.message + ' while trying to subscribe to:\n' + JSON.stringify(this.query, null, 4));
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
    if (typeof host !== "string" || (!host.startsWith("http://") && !host.startsWith("https://"))) {
        throw new Error("IHP Backend host url needs to start with \"http://\" or \"https://\", you passed \"" +  host + "\"")
    }
    if (host.endsWith('/')) {
        throw new Error('IHP Backend host url should not have a trailing slash, please remove the last \"/\" from \"' + host + "\"")
    }
    window.ihpBackendHost = host;
}


export { DataSyncController, DataSubscription, initIHPBackend };