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

/** Owns one backend count subscription and keeps it alive across reconnects. */
export class LiveCountStore implements ManagedExternalStore<number | null> {
    private readonly query: DynamicSQLQuery;
    private readonly controller: LiveCountController;
    private readonly listeners: Set<() => void> = new Set();
    private count: number | null = null;
    private subscriptionId: UUID | null = null;
    private createGeneration = 0;
    private isActive = true;
    private isStarted = false;

    constructor({ query, controller }: LiveCountStoreOptions) {
        this.query = query;
        this.controller = controller;
    }

    getSnapshot = (): number | null => this.count;

    subscribe = (listener: () => void): (() => void) => {
        this.listeners.add(listener);
        return () => {
            this.listeners.delete(listener);
        };
    };

    start(): void {
        if (this.isStarted || !this.isActive) {
            return;
        }

        this.isStarted = true;
        this.controller.addEventListener('message', this.onMessage);
        this.controller.addEventListener('close', this.onClose);
        this.controller.addEventListener('reconnect', this.onReconnect);
        void this.createSubscriptionOnServer();
    }

    dispose(): void {
        if (!this.isActive) {
            return;
        }

        this.isActive = false;
        this.createGeneration++;
        if (this.isStarted) {
            this.controller.removeEventListener('message', this.onMessage);
            this.controller.removeEventListener('close', this.onClose);
            this.controller.removeEventListener('reconnect', this.onReconnect);
        }
        this.listeners.clear();

        if (this.subscriptionId !== null) {
            const activeSubscriptionId = this.subscriptionId;
            this.subscriptionId = null;
            void this.deleteSubscriptionOnServer(activeSubscriptionId);
        }
    }

    private publish(count: number): void {
        this.count = count;
        for (const listener of this.listeners) {
            listener();
        }
    }

    private readonly onMessage: DataSyncEventMap['message'] = (message: ServerMessage) => {
        if (message.tag === 'DidChangeCount' && message.subscriptionId === this.subscriptionId) {
            this.publish(message.count as number);
        }
    };

    private readonly onClose: DataSyncEventMap['close'] = () => {
        this.createGeneration++;
        this.subscriptionId = null;
    };

    private readonly onReconnect: DataSyncEventMap['reconnect'] = () => {
        void this.createSubscriptionOnServer();
    };

    private async createSubscriptionOnServer(): Promise<void> {
        const generation = ++this.createGeneration;
        try {
            const response = await this.controller.sendMessage({ tag: 'CreateCountSubscription', query: this.query });
            const createdSubscriptionId = response.subscriptionId as UUID;

            if (!this.isActive || generation !== this.createGeneration) {
                await this.deleteSubscriptionOnServer(createdSubscriptionId);
                return;
            }

            this.subscriptionId = createdSubscriptionId;
            this.publish(response.count as number);
        } catch (error) {
            if (this.isActive && generation === this.createGeneration) {
                console.error('useCount: Failed to create count subscription', error);
            }
        }
    }

    private async deleteSubscriptionOnServer(subscriptionId: UUID): Promise<void> {
        try {
            await this.controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId });
        } catch (error) {
            console.error('useCount: Failed to delete count subscription', error);
        }
    }
}
