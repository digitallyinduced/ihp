import type { DataSyncController } from './ihp-datasync.js';
import type {
    SubscriptionTransport,
    SubscriptionTransportEvent,
} from './subscription-resource.js';
import type { ServerMessage } from './types.js';

/** Adapts the existing public controller class to the functional transport port. */
export function createControllerSubscriptionTransport(
    controller: DataSyncController,
    scopeKey: string,
): SubscriptionTransport {
    return Object.freeze({
        scopeKey,
        request: message => controller.sendMessage(message),
        isCurrent: () => controller.isBoundToTransportScope(scopeKey),
        subscribe(listener: (event: SubscriptionTransportEvent) => void): () => void {
            const onMessage = (message: ServerMessage): void => {
                listener({ type: 'message', message });
            };
            const onClose = (event: unknown): void => {
                listener({
                    type: 'closed',
                    scopeChanged: isTransportScopeChange(event),
                });
            };
            const onReconnect = (): void => {
                listener({ type: 'reconnected' });
            };
            controller.addEventListener('message', onMessage);
            controller.addEventListener('close', onClose);
            controller.addEventListener('reconnect', onReconnect);
            let subscribed = true;
            return () => {
                if (!subscribed) {
                    return;
                }
                subscribed = false;
                controller.removeEventListener('message', onMessage);
                controller.removeEventListener('close', onClose);
                controller.removeEventListener('reconnect', onReconnect);
            };
        },
    });
}

export function isTransportScopeChange(event: unknown): boolean {
    return typeof event === 'object'
        && event !== null
        && 'type' in event
        && (event as { type?: unknown }).type === 'transport-scope-changed';
}
