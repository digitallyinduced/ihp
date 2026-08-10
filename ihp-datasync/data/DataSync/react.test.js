import { CountSubscription } from './count-subscription.js';
import { DataSyncController } from './ihp-datasync.js';
import { jest } from '@jest/globals';

const countQuery = {
    table: 'tasks',
    selectedColumns: { tag: 'SelectAll' },
    whereCondition: null,
    orderByClause: [],
    distinctOnColumn: null,
    limit: null,
    offset: null,
};

describe('CountSubscription', () => {
    beforeEach(() => {
        DataSyncController.instance = null;
    });

    test('recreates its server subscription after a reconnect', async () => {
        const controller = DataSyncController.getInstance();
        let createCount = 0;
        controller.sendMessage = jest.fn(async payload => {
            if (payload.tag === 'CreateCountSubscription') {
                createCount++;
                return {
                    tag: 'DidCreateCountSubscription',
                    subscriptionId: `count-${createCount}`,
                    count: createCount * 10,
                };
            }
            return { tag: 'DidDeleteDataSubscription', subscriptionId: payload.subscriptionId };
        });

        const subscription = new CountSubscription(countQuery);
        const onStoreChange = jest.fn();
        const unsubscribe = subscription.subscribe(onStoreChange);
        await Promise.resolve();

        expect(subscription.getCount()).toBe(10);
        expect(subscription.subscriptionId).toBe('count-1');

        for (const listener of controller.eventListeners.close) {
            listener(null);
        }
        expect(subscription.getCount()).toBeNull();

        for (const listener of controller.eventListeners.reconnect) {
            listener();
        }
        await Promise.resolve();

        expect(subscription.getCount()).toBe(20);
        expect(subscription.subscriptionId).toBe('count-2');
        expect(controller.sendMessage).toHaveBeenCalledTimes(2);

        unsubscribe();
        await Promise.resolve();
    });

    test('ignores messages for an obsolete subscription id', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async () => ({
            tag: 'DidCreateCountSubscription',
            subscriptionId: 'current',
            count: 1,
        }));
        const subscription = new CountSubscription(countQuery);
        const unsubscribe = subscription.subscribe(() => {});
        await Promise.resolve();

        for (const listener of controller.eventListeners.message) {
            listener({ tag: 'DidChangeCount', subscriptionId: 'obsolete', count: 99 });
        }
        expect(subscription.getCount()).toBe(1);

        for (const listener of controller.eventListeners.message) {
            listener({ tag: 'DidChangeCount', subscriptionId: 'current', count: 2 });
        }
        expect(subscription.getCount()).toBe(2);

        unsubscribe();
    });
});
