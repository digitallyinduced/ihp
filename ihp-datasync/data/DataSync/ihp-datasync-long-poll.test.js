import { configureDataSyncTransport, DataSyncController, LongPollSocket } from './ihp-datasync.js';
import { jest } from '@jest/globals';

describe('DataSync long polling fallback', () => {
    let sockets;
    let requests;
    let controller;
    let originalGlobals;

    const respond = (request, status, body = {}) =>
        request.resolve({ ok: status >= 200 && status < 300, status, json: async () => body });
    const requestsTo = path => requests.filter(request => request.url === path);
    const settle = () => jest.advanceTimersByTimeAsync(0);

    beforeEach(() => {
        jest.useFakeTimers();
        jest.spyOn(console, 'log').mockImplementation(() => {});
        // The reconnect timer may race the retry loop and log a failed attempt
        jest.spyOn(console, 'error').mockImplementation(() => {});
        originalGlobals = Object.fromEntries(
            ['WebSocket', 'location', 'document', 'fetch'].map(key => [key, Object.getOwnPropertyDescriptor(globalThis, key)])
        );
        sockets = [];
        requests = [];
        globalThis.WebSocket = class {
            static OPEN = 1;
            readyState = 0;
            sent = [];
            constructor(url) { this.url = url; sockets.push(this); }
            send(message) { this.sent.push(message); }
            close() { this.readyState = 3; this.onclose?.({}); }
            open() { this.readyState = 1; return this.onopen({}); }
        };
        globalThis.fetch = (url, init) => new Promise((resolve, reject) => {
            init.signal.addEventListener('abort', () => reject(new Error('aborted')));
            requests.push({ url, body: init.body, resolve, reject });
        });
        globalThis.location = { protocol: 'https:' };
        globalThis.document = { location: { hostname: 'example.com', port: '443' } };
        DataSyncController.instance = null;
        configureDataSyncTransport(null);
        controller = DataSyncController.getInstance();
        controller.outbox.push('{"tag":"Queued"}');
    });

    afterEach(() => {
        controller.connection?.close();
        DataSyncController.instance = null;
        configureDataSyncTransport(null);
        for (const [key, descriptor] of Object.entries(originalGlobals)) {
            if (descriptor) Object.defineProperty(globalThis, key, descriptor);
            else delete globalThis[key];
        }
        jest.clearAllTimers();
        jest.useRealTimers();
        jest.restoreAllMocks();
    });

    // The WebSocket is refused, the long polling connection opens as c1
    async function openLongPoll() {
        const connecting = controller.startConnection();
        sockets[0].onerror({});
        await settle();
        respond(requestsTo('/DataSyncLongPoll')[0], 200, { connectionId: 'c1' });
        const socket = await connecting;
        await settle();
        return socket;
    }

    test('switches to long polling when the WebSocket cannot open', async () => {
        const socket = await openLongPoll();
        expect(socket).toBeInstanceOf(LongPollSocket);
        expect(controller.connection).toBe(socket);
        expect(requestsTo('/DataSyncLongPoll/c1/send').map(request => request.body)).toEqual(['[{"tag":"Queued"}]']);
        expect(requestsTo('/DataSyncLongPoll/c1/receive').map(request => request.body)).toEqual(['{"after":0}']);
    });

    test('treats a WebSocket that does not open within ten seconds as blocked', async () => {
        void controller.startConnection();
        await jest.advanceTimersByTimeAsync(10000);
        expect(sockets[0].readyState).toBe(3);
        expect(requestsTo('/DataSyncLongPoll')).toHaveLength(1);
    });

    test('delivers received messages and acknowledges them with the next receive', async () => {
        await openLongPoll();
        const received = [];
        controller.addEventListener('message', message => received.push(message));
        respond(requestsTo('/DataSyncLongPoll/c1/receive')[0], 200, {
            lastSequence: 2,
            closed: false,
            messages: [
                { tag: 'DidDelete', subscriptionId: 's', id: '1' },
                { tag: 'DidDelete', subscriptionId: 's', id: '2' },
            ],
        });
        await settle();
        expect(received.map(message => message.id)).toEqual(['1', '2']);
        expect(requestsTo('/DataSyncLongPoll/c1/receive').map(request => request.body)).toEqual(['{"after":0}', '{"after":2}']);
    });

    test('asks again for unacknowledged messages after a failed receive', async () => {
        await openLongPoll();
        requestsTo('/DataSyncLongPoll/c1/receive')[0].reject(new Error('cut off by a proxy'));
        await jest.advanceTimersByTimeAsync(1000);
        expect(requestsTo('/DataSyncLongPoll/c1/receive').map(request => request.body)).toEqual(['{"after":0}', '{"after":0}']);
        expect(controller.connection).not.toBeNull();
    });

    test('reconnects with long polling when the server no longer knows the connection', async () => {
        await openLongPoll();
        const closed = jest.fn();
        controller.addEventListener('close', closed);
        respond(requestsTo('/DataSyncLongPoll/c1/receive')[0], 404);
        await settle();
        expect(closed).toHaveBeenCalledTimes(1);
        expect(controller.connection).toBeNull();
        await jest.advanceTimersByTimeAsync(1000);
        expect(requestsTo('/DataSyncLongPoll')).toHaveLength(2);
        expect(sockets).toHaveLength(1);
    });

    test('drops the connection when posting messages fails', async () => {
        await openLongPoll();
        const closed = jest.fn();
        controller.addEventListener('close', closed);
        respond(requestsTo('/DataSyncLongPoll/c1/send')[0], 500);
        await settle();
        expect(closed).toHaveBeenCalledTimes(1);
        expect(requestsTo('/DataSyncLongPoll/c1/close')).toHaveLength(1);
    });

    test('keeps using WebSockets once a WebSocket has opened', async () => {
        const connecting = controller.startConnection();
        await sockets[0].open();
        await connecting;
        sockets[0].close();
        void controller.startConnection();
        sockets[1].onerror({});
        await jest.advanceTimersByTimeAsync(1000);
        expect(sockets).toHaveLength(3);
        expect(requests).toHaveLength(0);
    });

    test('returns to WebSockets when the server has no long polling endpoint', async () => {
        void controller.startConnection();
        sockets[0].onerror({});
        await settle();
        respond(requestsTo('/DataSyncLongPoll')[0], 404);
        await jest.advanceTimersByTimeAsync(2000);
        expect(sockets).toHaveLength(2);
        sockets[1].onerror({});
        await jest.advanceTimersByTimeAsync(4000);
        expect(sockets).toHaveLength(3);
        expect(requests).toHaveLength(1);
    });

    test('never switches a configured transport to long polling', async () => {
        configureDataSyncTransport({ url: () => 'wss://example.com/authenticated-datasync', authenticate: async () => {} });
        void controller.startConnection();
        sockets[0].onerror({});
        await jest.advanceTimersByTimeAsync(1000);
        expect(sockets).toHaveLength(2);
        expect(requests).toHaveLength(0);
    });
});
