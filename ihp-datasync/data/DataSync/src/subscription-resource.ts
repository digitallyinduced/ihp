import {
    initialSubscriptionState,
    transitionSubscription,
    type CreatedSubscription,
    type SubscriptionEvent,
    type SubscriptionPolicy,
    type SubscriptionPublication,
    type SubscriptionState,
} from './subscription-machine.js';
import type { ResourceSnapshot } from './subscription-reducer.js';
import type { ServerMessage } from './types.js';

export type SubscriptionTransportEvent =
    | Readonly<{ type: 'message'; message: ServerMessage }>
    | Readonly<{ type: 'closed'; scopeChanged: boolean }>
    | Readonly<{ type: 'reconnected' }>;

/** Minimal effect port between a subscription resource and the socket client. */
export type SubscriptionTransport = Readonly<{
    scopeKey: string;
    request(message: Record<string, unknown>): Promise<ServerMessage>;
    subscribe(listener: (event: SubscriptionTransportEvent) => void): () => void;
    isCurrent(): boolean;
}>;

export type SubscriptionProtocol<T> = Readonly<{
    create(transport: SubscriptionTransport): Promise<CreatedSubscription<T>>;
    delete(transport: SubscriptionTransport, subscriptionId: string): Promise<void>;
    refresh?(transport: SubscriptionTransport): Promise<T>;
    decodeMessage(
        message: ServerMessage,
        state: SubscriptionState<T>,
    ): SubscriptionEvent<T> | null;
    isRelevantMessage?(
        message: ServerMessage,
        state: SubscriptionState<T>,
    ): boolean;
    createError?(error: unknown): Error;
}>;

export type SubscriptionResourceEnvironment<T> = Readonly<{
    expectedScopeKey: string;
    currentScopeKey(): string;
    acquireTransport(): SubscriptionTransport;
    policy: SubscriptionPolicy;
    initialData?: T | null;
    cloneInitialResult?(value: T): T;
    writeCache?(value: T): void;
    evict?(): void;
    publish?(
        publication: SubscriptionPublication,
        state: SubscriptionState<T>,
        previousState: SubscriptionState<T>,
    ): void;
    onState?(state: SubscriptionState<T>, event: SubscriptionEvent<T>): void;
    reportError?(message: string, error: unknown): void;
    queueMicrotask?(callback: () => void): void;
}>;

export type SubscriptionResource<T> = Readonly<{
    getState(): SubscriptionState<T>;
    getSnapshot(): ResourceSnapshot<T>;
    updateDemand(subscribers: number, imperative: boolean): void;
    scheduleCloseIfUnused(): void;
    ensureCreated(): Promise<void>;
    close(): Promise<void>;
    receiveMessage(message: ServerMessage): void;
    transportClosed(scopeChanged?: boolean): void;
    transportReconnected(): Promise<void>;
    invalidateLegacy(): void;
    dispatchCompatibilityValue(value: T | null): void;
    dispatchCompatibilitySnapshot(value: T, revision?: number): void;
    initialResult: Promise<T>;
    resolveInitial(value: T): void;
    rejectInitial(error: Error): void;
}>;

type Deferred<T> = Readonly<{
    promise: Promise<T>;
    resolve(value: T): void;
    reject(error: Error): void;
}>;

/**
 * Builds the imperative shell around the pure subscription state machine.
 *
 * The closure owns effect handles, but every domain-state change is fed back
 * through `transitionSubscription`. Promise completions never mutate state.
 */
export function createSubscriptionResource<T>(
    protocol: SubscriptionProtocol<T>,
    environment: SubscriptionResourceEnvironment<T>,
): SubscriptionResource<T> {
    let state = initialSubscriptionState(environment.initialData);
    let transport: SubscriptionTransport | null = null;
    let unsubscribeTransport: (() => void) | null = null;
    const generationTransports = new Map<number, SubscriptionTransport>();
    const createTasks = new Map<number, Promise<void>>();
    const deleteTasks = new Set<Promise<void>>();
    const deleteTasksByKey = new Map<string, Promise<void>>();
    const eventQueue: SubscriptionEvent<T>[] = [];
    let processingEvents = false;
    const deferred = createDeferred<T>();
    void deferred.promise.catch(() => {});

    const report = (message: string, error: unknown): void => {
        try {
            environment.reportError?.(message, error);
        } catch (reportError) {
            console.error(message, error, reportError);
        }
    };

    const isScopeCurrent = (): boolean =>
        environment.currentScopeKey() === environment.expectedScopeKey;

    const detachTransport = (): void => {
        const unsubscribe = unsubscribeTransport;
        unsubscribeTransport = null;
        transport = null;
        if (unsubscribe !== null) {
            try {
                unsubscribe();
            } catch (error) {
                report('Failed to detach a subscription transport listener:', error);
            }
        }
    };

    const onTransportEvent = (event: SubscriptionTransportEvent): void => {
        switch (event.type) {
            case 'message':
                receiveMessage(event.message);
                return;
            case 'closed':
                if (state.phase.tag === 'live') {
                    generationTransports.delete(state.generation);
                }
                dispatch({
                    type: event.scopeChanged ? 'SCOPE_RETIRED' : 'TRANSPORT_CLOSED',
                });
                return;
            case 'reconnected':
                dispatch({ type: 'TRANSPORT_RECONNECTED' });
                return;
        }
    };

    const ensureTransport = (): SubscriptionTransport | null => {
        if (!isScopeCurrent()) {
            dispatch({ type: 'SCOPE_RETIRED' });
            return null;
        }
        if (transport !== null) {
            if (transport.scopeKey === environment.expectedScopeKey && transport.isCurrent()) {
                return transport;
            }
            dispatch({ type: 'SCOPE_RETIRED' });
            return null;
        }

        let acquired: SubscriptionTransport;
        try {
            acquired = environment.acquireTransport();
        } catch (error) {
            dispatch({
                type: 'CREATE_FAILED',
                generation: state.generation,
                error: toError(error),
            });
            return null;
        }
        if (
            acquired.scopeKey !== environment.expectedScopeKey
            || !isScopeCurrent()
            || !acquired.isCurrent()
        ) {
            dispatch({ type: 'SCOPE_RETIRED' });
            return null;
        }

        transport = acquired;
        try {
            unsubscribeTransport = acquired.subscribe(onTransportEvent);
        } catch (error) {
            transport = null;
            dispatch({
                type: 'CREATE_FAILED',
                generation: state.generation,
                error: toError(error),
            });
            return null;
        }
        return acquired;
    };

    const startCreate = (generation: number): void => {
        const createTransport = ensureTransport();
        if (createTransport === null) {
            return;
        }
        if (state.phase.tag !== 'creating' || state.generation !== generation) {
            return;
        }
        generationTransports.set(generation, createTransport);
        let task: Promise<void>;
        task = invokeEffect(() => protocol.create(createTransport)).then(created => {
            if (!isScopeCurrent() || !createTransport.isCurrent()) {
                dispatch({ type: 'SCOPE_RETIRED' });
            }
            dispatch({ type: 'CREATE_SUCCEEDED', generation, created });
            const isLiveCreate = state.scope === 'current'
                && state.generation === generation
                && state.phase.tag === 'live'
                && state.phase.subscriptionId === created.subscriptionId;
            if (!isLiveCreate) {
                return deleteTasksByKey.get(deleteTaskKey(generation, created.subscriptionId));
            }
        }).catch(error => {
            if (generationTransports.get(generation) === createTransport) {
                generationTransports.delete(generation);
            }
            if (
                state.scope !== 'current'
                || state.phase.tag !== 'creating'
                || state.generation !== generation
            ) {
                return;
            }
            let createError: Error;
            try {
                createError = protocol.createError?.(error) ?? toError(error);
            } catch (formatError) {
                createError = toError(formatError);
            }
            dispatch({ type: 'CREATE_FAILED', generation, error: createError });
            throw createError;
        }).finally(() => {
            if (createTasks.get(generation) === task) {
                createTasks.delete(generation);
            }
        });
        createTasks.set(generation, task);
        void task.catch(() => {});
    };

    const startDelete = (generation: number, subscriptionId: string): void => {
        const deleteTransport = generationTransports.get(generation) ?? transport;
        if (deleteTransport === null) {
            return;
        }
        const key = deleteTaskKey(generation, subscriptionId);
        let task: Promise<void>;
        task = invokeEffect(() => protocol.delete(deleteTransport, subscriptionId))
            .catch(error => {
                if (deleteTransport.isCurrent()) {
                    report('Failed to delete a stale subscription:', error);
                }
            })
            .finally(() => {
                deleteTasks.delete(task);
                if (deleteTasksByKey.get(key) === task) {
                    deleteTasksByKey.delete(key);
                }
                if (generationTransports.get(generation) === deleteTransport) {
                    generationTransports.delete(generation);
                }
            });
        deleteTasks.add(task);
        deleteTasksByKey.set(key, task);
    };

    const startRefresh = (generation: number): void => {
        const refresh = protocol.refresh;
        const refreshTransport = generationTransports.get(generation) ?? transport;
        if (refresh === undefined || refreshTransport === null) {
            dispatch({
                type: 'REFRESH_FAILED',
                generation,
                error: new Error('Subscription protocol does not support refresh'),
            });
            return;
        }
        if (!isScopeCurrent() || !refreshTransport.isCurrent()) {
            dispatch({ type: 'SCOPE_RETIRED' });
            return;
        }
        void invokeEffect(() => refresh(refreshTransport)).then(value => {
            if (!isScopeCurrent() || !refreshTransport.isCurrent()) {
                dispatch({ type: 'SCOPE_RETIRED' });
                return;
            }
            dispatch({ type: 'REFRESH_SUCCEEDED', generation, value });
        }).catch(error => {
            dispatch({ type: 'REFRESH_FAILED', generation, error: toError(error) });
        });
    };

    const interpretCommands = (
        commands: ReturnType<typeof transitionSubscription<T>>['commands'],
    ): void => {
        for (const command of commands) {
            switch (command.type) {
                case 'CREATE_REMOTE':
                    startCreate(command.generation);
                    break;
                case 'DELETE_REMOTE':
                    startDelete(command.generation, command.subscriptionId);
                    break;
                case 'REFRESH_REMOTE':
                    startRefresh(command.generation);
                    break;
                case 'QUEUE_IDLE_CHECK':
                    (environment.queueMicrotask ?? queueMicrotask)(() => {
                        dispatch({ type: 'IDLE_CHECK', token: command.token });
                    });
                    break;
                case 'CACHE_WRITE':
                    if (
                        isScopeCurrent()
                        && transport?.scopeKey === environment.expectedScopeKey
                        && transport.isCurrent()
                    ) {
                        try {
                            environment.writeCache?.(command.value);
                        } catch (error) {
                            report('Failed to cache a subscription snapshot:', error);
                        }
                    }
                    break;
                case 'SETTLE_INITIAL':
                    if (command.result.ok) {
                        deferred.resolve(
                            environment.cloneInitialResult?.(command.result.value)
                                ?? command.result.value,
                        );
                    } else {
                        deferred.reject(command.result.error);
                    }
                    break;
                case 'REPORT_REFRESH_FAILURE':
                    report('Failed to refresh a legacy subscription:', command.error);
                    break;
                case 'EVICT_FROM_REGISTRY':
                    try {
                        environment.evict?.();
                    } catch (error) {
                        report('Failed to evict a subscription resource:', error);
                    }
                    detachTransport();
                    break;
            }
        }
    };

    function dispatch(event: SubscriptionEvent<T>): void {
        eventQueue.push(event);
        if (processingEvents) {
            return;
        }
        processingEvents = true;
        try {
            while (eventQueue.length > 0) {
                const nextEvent = eventQueue.shift();
                if (nextEvent === undefined) {
                    continue;
                }
                const previousState = state;
                const transition = transitionSubscription(
                    previousState,
                    nextEvent,
                    environment.policy,
                );
                state = transition.state;
                if (state !== previousState) {
                    try {
                        environment.onState?.(state, nextEvent);
                    } catch (error) {
                        report('Subscription state observer failed:', error);
                    }
                }
                interpretCommands(transition.commands);
                const scopeRetirementQueued = eventQueue.some(
                    queuedEvent => queuedEvent.type === 'SCOPE_RETIRED',
                );
                if (transition.publication !== 'none' && !scopeRetirementQueued) {
                    try {
                        environment.publish?.(
                            transition.publication,
                            state,
                            previousState,
                        );
                    } catch (error) {
                        report('Subscription publication failed:', error);
                    }
                }
            }
        } finally {
            processingEvents = false;
        }
    }

    function receiveMessage(message: ServerMessage): void {
        try {
            if (protocol.isRelevantMessage?.(message, state) === false) {
                return;
            }
        } catch (error) {
            report('Failed to prefilter a subscription message:', error);
            return;
        }
        if (!isScopeCurrent() || transport?.isCurrent() === false) {
            dispatch({ type: 'SCOPE_RETIRED' });
            return;
        }
        let event: SubscriptionEvent<T> | null;
        try {
            event = protocol.decodeMessage(message, state);
        } catch (error) {
            report('Failed to decode a subscription message:', error);
            return;
        }
        if (event !== null) {
            dispatch(event);
        }
    }

    const updateDemand = (subscribers: number, imperative: boolean): void => {
        if ((subscribers > 0 || imperative) && !isScopeCurrent()) {
            dispatch({ type: 'SCOPE_RETIRED' });
            return;
        }
        dispatch({ type: 'DEMAND_CHANGED', subscribers, imperative });
    };

    const close = async (): Promise<void> => {
        dispatch({ type: 'CLOSE_REQUESTED' });
        // A consumer may call close() reentrantly from a publication callback.
        // In that case CLOSE_REQUESTED is queued until the current transition
        // finishes, so yield once before snapshotting the Delete tasks it
        // created. Normal non-reentrant closes have already dispatched here.
        await Promise.resolve();
        const tasks = Array.from(deleteTasks);
        if (tasks.length > 0) {
            await Promise.all(tasks);
        }
    };

    const ensureCreated = (): Promise<void> => {
        if (state.scope === 'stale') {
            return Promise.reject(new Error(environment.policy.scopeRetiredMessage));
        }
        if (state.phase.tag === 'live') {
            return Promise.resolve();
        }
        if (state.phase.tag === 'offline' || state.phase.tag === 'failed') {
            if (state.demand.subscribers > 0 || state.demand.imperative) {
                dispatch({ type: 'TRANSPORT_RECONNECTED' });
            }
        }
        if (state.phase.tag === 'creating') {
            return createTasks.get(state.generation) ?? Promise.resolve();
        }
        return Promise.resolve();
    };

    return Object.freeze({
        getState: () => state,
        getSnapshot: () => state.snapshot,
        updateDemand,
        scheduleCloseIfUnused: () => dispatch({ type: 'SCHEDULE_IDLE_CHECK' }),
        ensureCreated,
        close,
        receiveMessage,
        transportClosed: (scopeChanged = false) => dispatch({
            type: scopeChanged ? 'SCOPE_RETIRED' : 'TRANSPORT_CLOSED',
        }),
        transportReconnected: () => {
            dispatch({ type: 'TRANSPORT_RECONNECTED' });
            return createTasks.get(state.generation) ?? Promise.resolve();
        },
        invalidateLegacy: () => {
            if (!isScopeCurrent()) {
                dispatch({ type: 'SCOPE_RETIRED' });
                return;
            }
            if (state.phase.tag !== 'live') {
                return;
            }
            dispatch({
                type: 'LEGACY_INVALIDATED',
                generation: state.generation,
                subscriptionId: state.phase.subscriptionId,
            });
        },
        dispatchCompatibilityValue: value => dispatch({ type: 'COMPAT_VALUE_SET', value }),
        dispatchCompatibilitySnapshot: (value, revision) => {
            if (!isScopeCurrent()) {
                dispatch({ type: 'SCOPE_RETIRED' });
                return;
            }
            if (state.phase.tag === 'live') {
                dispatch({
                    type: 'SERVER_SNAPSHOT',
                    generation: state.generation,
                    subscriptionId: state.phase.subscriptionId,
                    value,
                    revision: revision ?? state.phase.revision + 1,
                });
                return;
            }
            dispatch({ type: 'COMPAT_VALUE_SET', value });
        },
        initialResult: deferred.promise,
        resolveInitial: deferred.resolve,
        rejectInitial: deferred.reject,
    });
}

function createDeferred<T>(): Deferred<T> {
    let settled = false;
    let resolvePromise!: (value: T) => void;
    let rejectPromise!: (error: Error) => void;
    const promise = new Promise<T>((resolve, reject) => {
        resolvePromise = resolve;
        rejectPromise = reject;
    });
    return Object.freeze({
        promise,
        resolve(value: T): void {
            if (!settled) {
                settled = true;
                resolvePromise(value);
            }
        },
        reject(error: Error): void {
            if (!settled) {
                settled = true;
                rejectPromise(error);
            }
        },
    });
}

function toError(error: unknown): Error {
    return error instanceof Error ? error : new Error(String(error));
}

function invokeEffect<T>(effect: () => Promise<T>): Promise<T> {
    try {
        return effect();
    } catch (error) {
        return Promise.reject(error);
    }
}

function deleteTaskKey(generation: number, subscriptionId: string): string {
    return `${generation}:${subscriptionId}`;
}
