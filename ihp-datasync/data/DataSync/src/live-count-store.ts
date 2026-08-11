import type { DataSyncController } from './ihp-datasync.js';
import type { ManagedExternalStore } from './external-store-registry.js';
import type { DataSyncEventMap, DynamicSQLQuery, ServerMessage, UUID } from './types.js';

type LiveCountController = Pick<
    DataSyncController,
    'sendMessage' | 'addEventListener' | 'removeEventListener'
>;

export type LiveCountStoreOptions = {
    query: DynamicSQLQuery;
    controller: LiveCountController;
};

export type LiveCountStore = ManagedExternalStore<number | null>;

/** Owns one backend count subscription and keeps it alive across reconnects. */
export function createLiveCountStore({ query, controller }: LiveCountStoreOptions): LiveCountStore {
    const listeners = new Set<() => void>();
    let count: number | null = null;
    let subscriptionId: UUID | null = null;
    let createGeneration = 0;
    let isActive = true;
    let isStarted = false;

    const getSnapshot = (): number | null => count;

    const subscribe = (listener: () => void): (() => void) => {
        listeners.add(listener);
        return () => {
            listeners.delete(listener);
        };
    };

    const publish = (nextCount: number): void => {
        count = nextCount;
        for (const listener of listeners) {
            listener();
        }
    };

    const deleteSubscriptionOnServer = async (subscriptionId: UUID): Promise<void> => {
        try {
            await controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId });
        } catch (error) {
            console.error('useCount: Failed to delete count subscription', error);
        }
    };

    const createSubscriptionOnServer = async (): Promise<void> => {
        const generation = ++createGeneration;
        try {
            const response = await controller.sendMessage({ tag: 'CreateCountSubscription', query });
            const createdSubscriptionId = response.subscriptionId as UUID;

            if (!isActive || generation !== createGeneration) {
                await deleteSubscriptionOnServer(createdSubscriptionId);
                return;
            }

            subscriptionId = createdSubscriptionId;
            publish(response.count as number);
        } catch (error) {
            if (isActive && generation === createGeneration) {
                console.error('useCount: Failed to create count subscription', error);
            }
        }
    };

    const onMessage: DataSyncEventMap['message'] = (message: ServerMessage) => {
        if (message.tag === 'DidChangeCount' && message.subscriptionId === subscriptionId) {
            publish(message.count as number);
        }
    };

    const onClose: DataSyncEventMap['close'] = () => {
        createGeneration++;
        subscriptionId = null;
    };

    const onReconnect: DataSyncEventMap['reconnect'] = () => {
        void createSubscriptionOnServer();
    };

    const start = (): void => {
        if (isStarted || !isActive) {
            return;
        }

        isStarted = true;
        controller.addEventListener('message', onMessage);
        controller.addEventListener('close', onClose);
        controller.addEventListener('reconnect', onReconnect);
        void createSubscriptionOnServer();
    };

    const dispose = (): void => {
        if (!isActive) {
            return;
        }

        isActive = false;
        createGeneration++;
        if (isStarted) {
            controller.removeEventListener('message', onMessage);
            controller.removeEventListener('close', onClose);
            controller.removeEventListener('reconnect', onReconnect);
        }
        listeners.clear();

        if (subscriptionId !== null) {
            const activeSubscriptionId = subscriptionId;
            subscriptionId = null;
            void deleteSubscriptionOnServer(activeSubscriptionId);
        }
    };

    return {
        getSnapshot,
        subscribe,
        start,
        dispose,
    };
}
