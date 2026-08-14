/**
 * Compile-only compatibility contract for the intentionally supported public
 * DataSync surface. Keep this narrower than the emitted class declarations:
 * optimistic-update implementation details are deliberately not API.
 */
import type {
    DataSubscription,
    DataSyncController,
    DataRecord,
    DataSubscriptionOptions,
    DynamicSQLQuery,
    QueryBuilder,
    ServerMessage,
    Transaction,
    UUID,
} from './index.js';
import type { CountSubscription, CountSubscriptionStore } from './react.js';
import type { DataSubscriptionStore as ReactDataSubscriptionStore } from './react.js';
import type { DataSubscriptionStore } from './data-subscription-store.js';

type Assert<T extends true> = T;
type Implements<Actual, Expected> = Actual extends Expected ? true : false;
type Equal<X, Y> =
    (<T>() => T extends X ? 1 : 2) extends
    (<T>() => T extends Y ? 1 : 2) ? true : false;
type IsWritable<T, K extends keyof T> = Equal<
    Pick<T, K>,
    { -readonly [P in K]: T[P] }
>;

type StableDataSubscription = {
    query: DynamicSQLQuery;
    records: DataRecord[] | null;
    subscribers: Array<(records: DataRecord[] | null) => void>;
    cache: Map<string, DataRecord[]> | null;
    newRecordBehaviour: number;
    subscriptionId: UUID | null;
    createOnServerPromise: Promise<DataRecord[]>;
    resolveCreateOnServer(value: DataRecord[]): void;
    rejectCreateOnServer(reason: Error): void;
    isClosed: boolean;
    isConnected: boolean;
    connectError: Error | null;
    onClose: () => void;
    getRecords(): DataRecord[] | null;
    subscribe(callback: (records: DataRecord[] | null) => void): () => void;
    createOnServer(): Promise<void>;
    close(): Promise<void>;
    scheduleCloseIfNotUsed(): void;
    closeIfNotUsed(): void;
    onMessage(message: ServerMessage): void;
    receiveUpdate(message: ServerMessage): void;
    onDataSyncClosed(event?: unknown): void;
    onDataSyncReconnect(): Promise<void>;
    detectNewRecordBehaviour(): number;
    onCreate(record: DataRecord, isOptimistic?: boolean): void;
    onUpdate(
        id: UUID,
        changeSet: Record<string, unknown> | null,
        appendSet: Record<string, unknown> | null,
        isOptimistic?: boolean,
    ): void;
    onDelete(id: UUID, isOptimistic?: boolean): void;
    onCreateOptimistic(record: DataRecord): void;
    supportsOptimisticUpdates(): boolean;
    updateSubscribers(): void;
};

type StableDataSyncController = {
    connection: WebSocket | null;
    dataSubscriptions: DataSubscription[];
    sendMessage(payload: Record<string, unknown>): Promise<ServerMessage>;
    addEventListener(event: 'message', callback: (message: ServerMessage) => void): void;
    removeEventListener(event: 'message', callback: (message: ServerMessage) => void): void;
};

type StableDataSyncControllerConstructor = {
    new(): DataSyncController;
    instance: DataSyncController | null;
    ihpBackendHost: string | null;
    getInstance(): DataSyncController;
    getWSUrl(): string;
    currentTransportScopeKey(): string;
    retireCurrentTransport(): void;
    authSessionDidChange(): void;
};

type StableDataSubscriptionConstructor = {
    new(
        query: DynamicSQLQuery,
        options?: DataSubscriptionOptions | null,
        cache?: Map<string, DataRecord[]> | null,
    ): DataSubscription;
};

type StableDataSubscriptionStore = {
    queryMap: Map<string, DataSubscription>;
    cache: Map<string, DataRecord[]>;
    get(query: DynamicSQLQuery, options?: DataSubscriptionOptions | null): DataSubscription;
};

type StableCountSubscription = {
    query: DynamicSQLQuery;
    count: number | null;
    subscriptionId: UUID | null;
    subscribers: Set<() => void>;
    getCount(): number | null;
    subscribe(callback: () => void): () => void;
};

type StableCountSubscriptionStore = {
    queryMap: Map<string, CountSubscription>;
    get(query: DynamicSQLQuery): CountSubscription;
};

type StableQueryBuilderSubscription = {
    subscribe(callback: (records: DataRecord[] | null) => void): () => void;
};

type StableReactEntrypoint = {
    AuthCompletedContext: unknown;
    AuthCompletedProvider: unknown;
    DataSubscriptionStore: typeof DataSubscriptionStore;
    CountSubscription: new(query: DynamicSQLQuery) => CountSubscription;
    CountSubscriptionStore: typeof CountSubscriptionStore;
    useQuery(
        queryBuilder: QueryBuilder<string, DataRecord>,
        options?: DataSubscriptionOptions | null,
    ): DataRecord[] | null;
    useQuerySingleResult(queryBuilder: QueryBuilder<string, DataRecord>): DataRecord | null;
    useCount(queryBuilder: QueryBuilder): number | null;
    useIsConnected(): boolean;
};

type _dataSubscriptionCompatibility = Assert<Implements<DataSubscription, StableDataSubscription>>;
type _controllerCompatibility = Assert<Implements<DataSyncController, StableDataSyncController>>;
type _controllerConstructorCompatibility = Assert<Implements<
    typeof import('./index.js').DataSyncController,
    StableDataSyncControllerConstructor
>>;
type _dataSubscriptionConstructorCompatibility = Assert<Implements<
    typeof import('./index.js').DataSubscription,
    StableDataSubscriptionConstructor
>>;
type _storeCompatibility = Assert<Implements<typeof DataSubscriptionStore, StableDataSubscriptionStore>>;
type _reactStoreCompatibility = Assert<Implements<typeof ReactDataSubscriptionStore, StableDataSubscriptionStore>>;
type _countCompatibility = Assert<Implements<CountSubscription, StableCountSubscription>>;
type _countStoreCompatibility = Assert<Implements<typeof CountSubscriptionStore, StableCountSubscriptionStore>>;
type _queryBuilderCompatibility = Assert<Implements<QueryBuilder<string, DataRecord>, StableQueryBuilderSubscription>>;
type _reactEntrypointCompatibility = Assert<Implements<typeof import('./react.js'), StableReactEntrypoint>>;
type _legacyReactEntrypointCompatibility = Assert<Implements<
    typeof import('./ihp-datasync-react.js'),
    Pick<StableReactEntrypoint, 'AuthCompletedContext' | 'AuthCompletedProvider' | 'useQuery'>
>>;

// Structural assignability does not catch an accidental `readonly` modifier,
// so lock the historically writable class fields explicitly.
type _writableSubscriptionQuery = Assert<IsWritable<DataSubscription, 'query'>>;
type _writableSubscriptionRecords = Assert<IsWritable<DataSubscription, 'records'>>;
type _writableSubscriptionSubscribers = Assert<IsWritable<DataSubscription, 'subscribers'>>;
type _writableSubscriptionCache = Assert<IsWritable<DataSubscription, 'cache'>>;
type _writableSubscriptionBehaviour = Assert<IsWritable<DataSubscription, 'newRecordBehaviour'>>;
type _writableSubscriptionId = Assert<IsWritable<DataSubscription, 'subscriptionId'>>;
type _writableCreatePromise = Assert<IsWritable<DataSubscription, 'createOnServerPromise'>>;
type _writableClosedFlag = Assert<IsWritable<DataSubscription, 'isClosed'>>;
type _writableConnectedFlag = Assert<IsWritable<DataSubscription, 'isConnected'>>;
type _writableConnectError = Assert<IsWritable<DataSubscription, 'connectError'>>;
type _writableOnClose = Assert<IsWritable<DataSubscription, 'onClose'>>;
type _writableResolveCreate = Assert<IsWritable<DataSubscription, 'resolveCreateOnServer'>>;
type _writableRejectCreate = Assert<IsWritable<DataSubscription, 'rejectCreateOnServer'>>;
type _writableCountQuery = Assert<IsWritable<CountSubscription, 'query'>>;
type _writableCount = Assert<IsWritable<CountSubscription, 'count'>>;
type _writableCountId = Assert<IsWritable<CountSubscription, 'subscriptionId'>>;
type _writableCountSubscribers = Assert<IsWritable<CountSubscription, 'subscribers'>>;
type _writableBuilderQuery = Assert<IsWritable<QueryBuilder, 'query'>>;
type _writableBuilderTransaction = Assert<IsWritable<QueryBuilder, 'transactionId'>>;
type _writableTransactionId = Assert<IsWritable<Transaction, 'transactionId'>>;
type _writableTransactionController = Assert<IsWritable<Transaction, 'dataSyncController'>>;
type _writableControllerConnection = Assert<IsWritable<DataSyncController, 'connection'>>;
type _writableControllerSubscriptions = Assert<IsWritable<DataSyncController, 'dataSubscriptions'>>;
type _writableStoreQueryMap = Assert<IsWritable<typeof DataSubscriptionStore, 'queryMap'>>;
type _writableCountStoreQueryMap = Assert<IsWritable<typeof CountSubscriptionStore, 'queryMap'>>;
