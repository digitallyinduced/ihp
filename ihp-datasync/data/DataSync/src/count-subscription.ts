import { DataSyncController } from './ihp-datasync.js';
import type { DynamicSQLQuery, ServerMessage } from './types.js';

/** Maintains the server-side subscription used by the React useCount hook. */
export class CountSubscription {
    query: DynamicSQLQuery;
    count: number | null;
    subscriptionId: string | null;
    subscribers: Set<() => void>;
    private controller: DataSyncController;
    private isStarted: boolean;
    private generation: number;

    constructor(query: DynamicSQLQuery) {
        this.query = query;
        this.count = null;
        this.subscriptionId = null;
        this.subscribers = new Set();
        this.controller = DataSyncController.getInstance();
        this.isStarted = false;
        this.generation = 0;

        this.subscribe = this.subscribe.bind(this);
        this.getCount = this.getCount.bind(this);
        this.onMessage = this.onMessage.bind(this);
        this.onDataSyncClosed = this.onDataSyncClosed.bind(this);
        this.onDataSyncReconnect = this.onDataSyncReconnect.bind(this);
    }

    subscribe(callback: () => void): () => void {
        this.subscribers.add(callback);
        if (!this.isStarted) {
            this.start();
        }

        return () => {
            this.subscribers.delete(callback);
            if (this.subscribers.size === 0) {
                this.stop();
            }
        };
    }

    getCount(): number | null {
        return this.count;
    }

    private start(): void {
        this.isStarted = true;
        this.controller.addEventListener('message', this.onMessage);
        this.controller.addEventListener('close', this.onDataSyncClosed);
        this.controller.addEventListener('reconnect', this.onDataSyncReconnect);
        void this.createOnServer();
    }

    private stop(): void {
        if (!this.isStarted) {
            return;
        }

        this.isStarted = false;
        this.generation++;
        const subscriptionId = this.subscriptionId;
        this.subscriptionId = null;
        this.controller.removeEventListener('message', this.onMessage);
        this.controller.removeEventListener('close', this.onDataSyncClosed);
        this.controller.removeEventListener('reconnect', this.onDataSyncReconnect);

        if (subscriptionId !== null) {
            void this.deleteOnServer(subscriptionId);
        }
    }

    private async createOnServer(): Promise<void> {
        const generation = ++this.generation;
        try {
            const response = await this.controller.sendMessage({ tag: 'CreateCountSubscription', query: this.query });
            const subscriptionId = response.subscriptionId as string;

            if (!this.isStarted || generation !== this.generation) {
                await this.deleteOnServer(subscriptionId);
                return;
            }

            this.subscriptionId = subscriptionId;
            this.count = response.count as number;
            this.notifySubscribers();
        } catch (error) {
            if (this.isStarted && generation === this.generation) {
                console.error('useCount: Failed to create count subscription', error);
            }
        }
    }

    private async deleteOnServer(subscriptionId: string): Promise<void> {
        try {
            await this.controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId });
        } catch (error) {
            if (this.controller.connection !== null) {
                console.error('useCount: Failed to delete count subscription', error);
            }
        }
    }

    private onMessage(message: ServerMessage): void {
        if (message.tag === 'DidChangeCount' && message.subscriptionId === this.subscriptionId) {
            this.count = message.count as number;
            this.notifySubscribers();
        }
    }

    private onDataSyncClosed(): void {
        this.generation++;
        this.subscriptionId = null;
        this.count = null;
        this.notifySubscribers();
    }

    private onDataSyncReconnect(): void {
        if (this.isStarted) {
            void this.createOnServer();
        }
    }

    private notifySubscribers(): void {
        for (const subscriber of this.subscribers) {
            subscriber();
        }
    }
}
