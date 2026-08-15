import type { DataSyncController } from './ihp-datasync.js';
import type { ManagedExternalStore } from './external-store-registry.js';
import type { DataSyncEventMap, ServerMessage, UUID } from './types.js';

export type LiveSubscriptionController = Pick<
    DataSyncController,
    'sendMessage' | 'addEventListener' | 'removeEventListener'
>;

type StoreContext<TSnapshot> = {
    getSnapshot: () => TSnapshot;
    publish: (snapshot: TSnapshot) => void;
};

type SubscriptionHandlers = {
    createOnServer: () => Promise<ServerMessage>;
    onCreate: (response: ServerMessage) => void;
    onMessage: (message: ServerMessage) => void;
    onCreateError: (error: unknown) => void;
    onDeleteError: (error: unknown) => void;
    onDispose?: () => void;
};

/** Creates a reconnecting backend subscription exposed as an external store. */
export function createLiveSubscriptionStore<TSnapshot>(
    controller: LiveSubscriptionController,
    initialSnapshot: TSnapshot,
    setup: (context: StoreContext<TSnapshot>) => SubscriptionHandlers,
): ManagedExternalStore<TSnapshot> {
    const listeners = new Set<() => void>();
    let snapshot = initialSnapshot;
    let subscriptionId: UUID | null = null;
    let createGeneration = 0;
    let isActive = true;

    const getSnapshot = (): TSnapshot => snapshot;
    const publish = (nextSnapshot: TSnapshot): void => {
        snapshot = nextSnapshot;
        for (const listener of listeners) {
            listener();
        }
    };
    const handlers = setup({ getSnapshot, publish });

    const deleteFromServer = async (id: UUID): Promise<void> => {
        try {
            await controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId: id });
        } catch (error) {
            handlers.onDeleteError(error);
        }
    };

    const createOnServer = async (): Promise<void> => {
        const generation = ++createGeneration;
        try {
            const response = await handlers.createOnServer();
            const createdSubscriptionId = response.subscriptionId as UUID;
            if (!isActive || generation !== createGeneration) {
                await deleteFromServer(createdSubscriptionId);
                return;
            }

            subscriptionId = createdSubscriptionId;
            handlers.onCreate(response);
        } catch (error) {
            if (isActive && generation === createGeneration) {
                handlers.onCreateError(error);
            }
        }
    };

    const onMessage: DataSyncEventMap['message'] = message => {
        if (message.subscriptionId === subscriptionId) {
            handlers.onMessage(message);
        }
    };
    const onClose: DataSyncEventMap['close'] = () => {
        createGeneration++;
        subscriptionId = null;
    };
    const onReconnect: DataSyncEventMap['reconnect'] = () => {
        void createOnServer();
    };

    const subscribe = (listener: () => void): (() => void) => {
        listeners.add(listener);
        return () => {
            listeners.delete(listener);
        };
    };
    const dispose = (): void => {
        if (!isActive) {
            return;
        }

        isActive = false;
        createGeneration++;
        controller.removeEventListener('message', onMessage);
        controller.removeEventListener('close', onClose);
        controller.removeEventListener('reconnect', onReconnect);
        handlers.onDispose?.();
        listeners.clear();

        if (subscriptionId !== null) {
            const activeSubscriptionId = subscriptionId;
            subscriptionId = null;
            void deleteFromServer(activeSubscriptionId);
        }
    };

    controller.addEventListener('message', onMessage);
    controller.addEventListener('close', onClose);
    controller.addEventListener('reconnect', onReconnect);
    void createOnServer();

    return {
        getSnapshot,
        subscribe,
        dispose,
    };
}
