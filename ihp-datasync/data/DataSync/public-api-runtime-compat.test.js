import * as root from './index.js';
import * as dataSyncEntrypoint from './ihp-datasync.js';
import * as queryBuilderEntrypoint from './ihp-querybuilder.js';
import * as transactionEntrypoint from './transaction.js';
import * as reactEntrypoint from './react.js';
import * as legacyReactEntrypoint from './ihp-datasync-react.js';
import * as dataSubscriptionStoreEntrypoint from './data-subscription-store.js';
import * as countSubscriptionEntrypoint from './count-subscription.js';

function query(overrides = {}) {
    return {
        table: 'tasks',
        selectedColumns: { tag: 'SelectAll' },
        whereCondition: null,
        orderByClause: [],
        distinctOnColumn: null,
        limit: null,
        offset: null,
        ...overrides,
    };
}

let originalControllerInstance;
let originalBackendHost;
let originalDataSubscriptionQueryMap;
let originalDataSubscriptionCache;
let originalCountSubscriptionQueryMap;

beforeEach(() => {
    originalControllerInstance = root.DataSyncController.instance;
    originalBackendHost = root.DataSyncController.ihpBackendHost;
    originalDataSubscriptionQueryMap = dataSubscriptionStoreEntrypoint.DataSubscriptionStore.queryMap;
    originalDataSubscriptionCache = dataSubscriptionStoreEntrypoint.DataSubscriptionStore.cache;
    originalCountSubscriptionQueryMap = countSubscriptionEntrypoint.CountSubscriptionStore.queryMap;

    root.DataSyncController.instance = null;
    root.DataSyncController.ihpBackendHost = null;
});

afterEach(() => {
    root.DataSyncController.instance = originalControllerInstance;
    root.DataSyncController.ihpBackendHost = originalBackendHost;
    dataSubscriptionStoreEntrypoint.DataSubscriptionStore.queryMap = originalDataSubscriptionQueryMap;
    dataSubscriptionStoreEntrypoint.DataSubscriptionStore.cache = originalDataSubscriptionCache;
    countSubscriptionEntrypoint.CountSubscriptionStore.queryMap = originalCountSubscriptionQueryMap;
});

describe('DataSync public runtime compatibility', () => {
    test('root and documented deep entrypoints expose the same runtime values', () => {
        expect(root.DataSyncController).toBe(dataSyncEntrypoint.DataSyncController);
        expect(root.DataSubscription).toBe(dataSyncEntrypoint.DataSubscription);
        expect(root.QueryBuilder).toBe(queryBuilderEntrypoint.QueryBuilder);
        expect(root.ConditionBuilder).toBe(queryBuilderEntrypoint.ConditionBuilder);
        expect(root.Transaction).toBe(transactionEntrypoint.Transaction);
        expect(root.withTransaction).toBe(transactionEntrypoint.withTransaction);

        expect(reactEntrypoint.DataSubscriptionStore)
            .toBe(dataSubscriptionStoreEntrypoint.DataSubscriptionStore);
        expect(reactEntrypoint.CountSubscription)
            .toBe(countSubscriptionEntrypoint.CountSubscription);
        expect(reactEntrypoint.CountSubscriptionStore)
            .toBe(countSubscriptionEntrypoint.CountSubscriptionStore);
        expect(legacyReactEntrypoint.AuthCompletedContext)
            .toBe(reactEntrypoint.AuthCompletedContext);
        expect(legacyReactEntrypoint.AuthCompletedProvider)
            .toBe(reactEntrypoint.AuthCompletedProvider);
        expect(legacyReactEntrypoint.useQuery).toBe(reactEntrypoint.useQuery);
    });

    test('public constructors retain instanceof and prototype method behaviour', () => {
        const controller = new root.DataSyncController();
        const subscription = new root.DataSubscription(query());
        const countSubscription = new reactEntrypoint.CountSubscription(query());
        const queryBuilder = new root.QueryBuilder('tasks');
        const conditionBuilder = new root.ConditionBuilder();
        const transaction = new root.Transaction();

        expect(controller).toBeInstanceOf(root.DataSyncController);
        expect(subscription).toBeInstanceOf(root.DataSubscription);
        expect(countSubscription).toBeInstanceOf(reactEntrypoint.CountSubscription);
        expect(queryBuilder).toBeInstanceOf(root.QueryBuilder);
        expect(conditionBuilder).toBeInstanceOf(root.ConditionBuilder);
        expect(transaction).toBeInstanceOf(root.Transaction);

        expect(typeof root.DataSyncController.prototype.sendMessage).toBe('function');
        expect(typeof root.DataSyncController.prototype.addEventListener).toBe('function');
        expect(typeof root.DataSubscription.prototype.subscribe).toBe('function');
        expect(typeof root.DataSubscription.prototype.createOnServer).toBe('function');
        expect(typeof reactEntrypoint.CountSubscription.prototype.subscribe).toBe('function');
        expect(typeof root.QueryBuilder.prototype.fetch).toBe('function');
        expect(typeof root.QueryBuilder.prototype.subscribe).toBe('function');
        expect(typeof root.ConditionBuilder.prototype.where).toBe('function');
        expect(typeof root.Transaction.prototype.start).toBe('function');
        expect(typeof root.Transaction.prototype.commit).toBe('function');

        expect(Object.getOwnPropertyDescriptor(root.DataSubscription.prototype, 'records')?.get)
            .toEqual(expect.any(Function));
        expect(Object.getOwnPropertyDescriptor(root.DataSubscription.prototype, 'records')?.set)
            .toEqual(expect.any(Function));
        expect(Object.getOwnPropertyDescriptor(reactEntrypoint.CountSubscription.prototype, 'count')?.get)
            .toEqual(expect.any(Function));
        expect(Object.getOwnPropertyDescriptor(reactEntrypoint.CountSubscription.prototype, 'count')?.set)
            .toEqual(expect.any(Function));
    });

    test('historically writable static and public fields remain writable', () => {
        const controller = new root.DataSyncController();
        const subscription = new root.DataSubscription(query());
        const countSubscription = new reactEntrypoint.CountSubscription(query());
        const queryBuilder = new root.QueryBuilder('tasks');
        const transaction = new root.Transaction();

        root.DataSyncController.instance = controller;
        root.DataSyncController.ihpBackendHost = 'https://api.example.test';
        expect(root.DataSyncController.instance).toBe(controller);
        expect(root.DataSyncController.ihpBackendHost).toBe('https://api.example.test');

        const socket = { send() {}, close() {} };
        controller.connection = socket;
        controller.dataSubscriptions = [subscription];
        expect(controller.connection).toBe(socket);
        expect(controller.dataSubscriptions).toEqual([subscription]);

        const replacementQuery = query({ table: 'other_tasks' });
        const subscriber = () => {};
        const cache = new Map();
        subscription.query = replacementQuery;
        subscription.records = [{ id: 'task-1' }];
        subscription.subscribers = [subscriber];
        subscription.cache = cache;
        subscription.subscriptionId = 'subscription-id';
        expect(subscription.query).toBe(replacementQuery);
        expect(subscription.records).toEqual([{ id: 'task-1' }]);
        expect(subscription.subscribers).toEqual([subscriber]);
        expect(subscription.cache).toBe(cache);
        expect(subscription.subscriptionId).toBe('subscription-id');

        const countSubscribers = new Set([subscriber]);
        countSubscription.query = replacementQuery;
        countSubscription.count = 3;
        countSubscription.subscriptionId = 'count-subscription-id';
        countSubscription.subscribers = countSubscribers;
        expect(countSubscription.query).toBe(replacementQuery);
        expect(countSubscription.count).toBe(3);
        expect(countSubscription.subscriptionId).toBe('count-subscription-id');
        expect(countSubscription.subscribers).toBe(countSubscribers);

        queryBuilder.query = replacementQuery;
        queryBuilder.transactionId = 'query-transaction-id';
        transaction.transactionId = 'transaction-id';
        transaction.dataSyncController = controller;
        expect(queryBuilder.query).toBe(replacementQuery);
        expect(queryBuilder.transactionId).toBe('query-transaction-id');
        expect(transaction.transactionId).toBe('transaction-id');
        expect(transaction.dataSyncController).toBe(controller);
    });

    test('public stores retain their Map-shaped inspection and replacement surfaces', () => {
        const dataStore = dataSubscriptionStoreEntrypoint.DataSubscriptionStore;
        const countStore = countSubscriptionEntrypoint.CountSubscriptionStore;
        const replacementQueryMap = new Map();
        const replacementCache = new Map();
        const replacementCountQueryMap = new Map();

        dataStore.queryMap = replacementQueryMap;
        dataStore.cache = replacementCache;
        expect(dataStore.queryMap).toBe(replacementQueryMap);
        expect(dataStore.cache).toBe(replacementCache);
        expect(dataStore.queryMap).toBeInstanceOf(Map);
        expect(dataStore.cache).toBeInstanceOf(Map);

        countStore.queryMap = replacementCountQueryMap;
        expect(countStore.queryMap).toBe(replacementCountQueryMap);
        expect(countStore.queryMap).toBeInstanceOf(Map);

        const key = '__public_runtime_compatibility__';
        const hadPreviousValue = countStore.queryMap.has(key);
        const previousValue = countStore.queryMap.get(key);
        const countSubscription = new reactEntrypoint.CountSubscription(query());
        try {
            countStore.queryMap.set(key, countSubscription);
            expect(countStore.queryMap.get(key)).toBe(countSubscription);
            expect(countStore.queryMap.delete(key)).toBe(true);
            expect(countStore.queryMap.has(key)).toBe(false);
        } finally {
            if (hadPreviousValue) {
                countStore.queryMap.set(key, previousValue);
            } else {
                countStore.queryMap.delete(key);
            }
        }
    });
});
