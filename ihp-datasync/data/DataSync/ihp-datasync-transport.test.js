import { configureDataSyncTransport, DataSyncController } from './ihp-datasync.js';
import { jest } from '@jest/globals';

describe('DataSync authenticated transport', () => {
    let sockets;
    let controller;
    let originalGlobals;

    beforeEach(() => {
        jest.useFakeTimers();
        jest.spyOn(console, 'log').mockImplementation(() => {});
        originalGlobals = Object.fromEntries(
            ['WebSocket', 'location', 'document'].map(key => [key, Object.getOwnPropertyDescriptor(globalThis, key)])
        );
        sockets = [];
        globalThis.WebSocket = class {
            static OPEN = 1;
            readyState = 0;
            sent = [];
            constructor(url) { this.url = url; sockets.push(this); }
            send(message) { this.sent.push(message); }
            close() { this.readyState = 3; this.onclose?.({}); }
            open() { this.readyState = 1; return this.onopen({}); }
        };
        globalThis.location = { protocol: 'https:' };
        globalThis.document = { location: { hostname: 'example.com', port: '443' } };
        DataSyncController.instance = null;
        configureDataSyncTransport(null);
        controller = DataSyncController.getInstance();
        controller.outbox.push('queued request');
    });

    afterEach(() => {
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

    test('preserves the default cookie transport', async () => {
        const connecting = controller.startConnection();
        await sockets[0].open();
        await connecting;
        expect(sockets[0].url).toBe('wss://example.com:443/DataSyncController');
        expect(sockets[0].sent).toEqual(['queued request']);
    });

    test('waits for authentication before publishing or flushing', async () => {
        let authorize;
        configureDataSyncTransport({
            url: () => 'wss://example.com/authenticated-datasync',
            authenticate: socket => {
                socket.send('authentication frame');
                return new Promise(resolve => { authorize = resolve; });
            },
        });
        const opened = jest.fn();
        controller.addEventListener('open', opened);
        const connecting = controller.startConnection();
        const opening = sockets[0].open();
        expect(sockets[0].url).toBe('wss://example.com/authenticated-datasync');
        expect(controller.connection).toBeNull();
        expect(opened).not.toHaveBeenCalled();
        expect(sockets[0].sent).toEqual(['authentication frame']);
        expect(() => configureDataSyncTransport(null)).toThrow('cannot change');
        authorize();
        await opening;
        await connecting;
        expect(opened).toHaveBeenCalledTimes(1);
        expect(sockets[0].sent).toEqual(['authentication frame', 'queued request']);
        expect(() => configureDataSyncTransport(null)).toThrow('cannot change');
        expect(jest.getTimerCount()).toBe(0);
    });

    test.each(['close', 'error', 'timeout', 'rejection'])(
        '%s rejects the pending attempt and late authentication cannot flush',
        async event => {
            let authorize;
            let rejectAuthentication;
            configureDataSyncTransport({
                url: () => 'wss://example.com/authenticated-datasync',
                authenticate: () => new Promise((resolve, reject) => {
                    authorize = resolve;
                    rejectAuthentication = reject;
                }),
            });
            // Leave the retry delay suspended; no real connections or timers.
            void controller.startConnection();
            const rejected = expect(controller.pendingConnection)
                .rejects.toThrow('DataSync connection or authentication failed');
            const opening = sockets[0].open();
            if (event === 'close') sockets[0].close();
            if (event === 'error') sockets[0].onerror({});
            if (event === 'timeout') jest.advanceTimersByTime(10000);
            if (event === 'rejection') rejectAuthentication(new Error('secret protocol data'));
            await rejected;
            authorize();
            await opening;
            expect(sockets[0].readyState).toBe(3);
            expect(controller.connection).toBeNull();
            expect(sockets[0].sent).toEqual([]);
            expect(controller.outbox).toEqual(['queued request']);
        }
    );

    test('authenticates each connection again after disconnecting', async () => {
        const authenticate = jest.fn(async () => {});
        configureDataSyncTransport({ url: () => 'wss://example.com/datasync', authenticate });
        const connecting = controller.startConnection();
        await sockets[0].open();
        await connecting;
        sockets[0].close();
        const reconnecting = controller.startConnection();
        await sockets[1].open();
        await reconnecting;
        expect(authenticate).toHaveBeenCalledTimes(2);
        expect(authenticate).toHaveBeenNthCalledWith(2, sockets[1]);
    });

    test('clears exhausted attempts so a subsequent connection can start', async () => {
        configureDataSyncTransport({
            url: () => 'wss://example.com/datasync',
            authenticate: async () => { throw new Error('authentication denied'); },
        });
        const connecting = controller.startConnection();
        const rejected = expect(connecting).rejects.toThrow('Unable to connect');
        for (let attempt = 0; attempt < 32; attempt++) {
            await sockets[attempt].open();
            await jest.advanceTimersByTimeAsync(2 ** Math.min(attempt, 6) * 1000);
        }
        await rejected;
        expect(controller.pendingConnection).toBeNull();
        configureDataSyncTransport(null);
        const retrying = controller.startConnection();
        await sockets[32].open();
        await retrying;
        expect(sockets[32].sent).toEqual(['queued request']);
    });
});
