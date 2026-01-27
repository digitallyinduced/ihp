var debugMode = document.currentScript.dataset.debugMode || false;

// Connection states
var ConnectionState = {
    CONNECTING: 'connecting',
    CONNECTED: 'connected',
    DISCONNECTED: 'disconnected',
    RECONNECTING: 'reconnecting',
    FAILED: 'failed'
};

// Configuration
var SSCConfig = {
    maxReconnectAttempts: 10,
    initialReconnectDelay: 1000,  // 1 second
    maxReconnectDelay: 30000,     // 30 seconds
    reconnectBackoffMultiplier: 1.5,
    maxActionQueueSize: 100       // Maximum queued actions while disconnected
};

window.callServerAction = function (action, payload) {
    var component = findActiveComponent(event.target || document.activeElement);
    var message = JSON.stringify({ action: action, ...(payload || {}) });

    // Check connection exists and is connected
    if (component.connection && component.connectionState === ConnectionState.CONNECTED) {
        component.connection.send(message);
    } else {
        // Queue action for replay when reconnected
        component.actionQueue = component.actionQueue || [];
        // Limit queue size to prevent memory issues
        if (component.actionQueue.length < SSCConfig.maxActionQueueSize) {
            component.actionQueue.push(message);
            if (debugMode) console.log('Action queued (not connected):', action);
        } else {
            if (debugMode) console.warn('Action queue full, dropping action:', action);
        }
    }
}


if (window.Turbolinks) {
    document.addEventListener('turbolinks:load', initializeAllSSCComponents);
} else {
    initializeAllSSCComponents();
}

// Reconnect when tab becomes visible
document.addEventListener('visibilitychange', function() {
    if (document.visibilityState === 'visible') {
        for (var component of document.querySelectorAll('.ihp-ssc')) {
            if (component.connectionState === ConnectionState.DISCONNECTED ||
                component.connectionState === ConnectionState.FAILED) {
                if (debugMode) console.log('Tab visible, attempting reconnect');
                reconnectSSC(component);
            }
        }
    }
});

function initializeAllSSCComponents() {
    for (var component of document.querySelectorAll('.ihp-ssc')) {
        if (debugMode) console.log('init', component)
        initializeSSC(component);
    }
}

function initializeSSC(component) {
    component.connectionState = ConnectionState.CONNECTING;
    component.reconnectAttempts = 0;
    component.actionQueue = [];

    connectSSC(component);
}

function connectSSC(component) {
    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    var socketHost = socketProtocol + "://" + window.location.hostname + ":" + document.location.port;

    updateConnectionUI(component, ConnectionState.CONNECTING);

    try {
        component.connection = new WebSocket(socketHost + component.dataset.path);
    } catch (e) {
        if (debugMode) console.error('WebSocket creation failed:', e);
        handleConnectionFailure(component);
        return;
    }

    component.connection.onopen = function (event) {
        if (debugMode) console.log('Connected');
        component.connectionState = ConnectionState.CONNECTED;
        component.reconnectAttempts = 0;
        updateConnectionUI(component, ConnectionState.CONNECTED);

        // Replay queued actions
        if (component.actionQueue && component.actionQueue.length > 0) {
            if (debugMode) console.log('Replaying', component.actionQueue.length, 'queued actions');
            for (var message of component.actionQueue) {
                component.connection.send(message);
            }
            component.actionQueue = [];
        }
    };

    component.connection.onclose = function (event) {
        if (debugMode) console.log('Connection closed:', event.code, event.reason);
        component.connectionState = ConnectionState.DISCONNECTED;
        scheduleReconnect(component);
    };

    component.connection.onerror = function (event) {
        if (debugMode) console.error('WebSocket error:', event);
        // onclose will be called after this, so we don't need to handle reconnection here
    };

    component.connection.onmessage = function (event) {
        var payload = JSON.parse(event.data);

        // Check if this is an error message from the server
        if (payload.type && payload.type.endsWith('Error')) {
            handleServerError(component, payload);
            return;
        }

        evaluatePatch(component, payload);
        if (debugMode) console.log(payload);
    };
}

function handleServerError(component, error) {
    if (debugMode) console.error('Server error:', error);

    // Dispatch custom event for application-level error handling
    var errorEvent = new CustomEvent('ssc:error', {
        bubbles: true,
        detail: { error: error, component: component }
    });
    component.dispatchEvent(errorEvent);

    // Show error indicator but keep component functional
    var existingIndicator = component.querySelector('.ssc-error-indicator');
    if (!existingIndicator) {
        var indicator = document.createElement('div');
        indicator.className = 'ssc-error-indicator';
        indicator.style.cssText = 'background: #fee2e2; color: #991b1b; padding: 8px 12px; border-radius: 4px; margin-bottom: 8px; font-size: 14px;';
        indicator.textContent = 'Error: ' + (error.errorMessage || 'An error occurred');
        component.insertBefore(indicator, component.firstChild);

        // Auto-remove after 5 seconds
        setTimeout(function() {
            if (indicator.parentNode) {
                indicator.remove();
            }
        }, 5000);
    }
}

function scheduleReconnect(component) {
    if (component.reconnectAttempts >= SSCConfig.maxReconnectAttempts) {
        if (debugMode) console.log('Max reconnect attempts reached');
        component.connectionState = ConnectionState.FAILED;
        updateConnectionUI(component, ConnectionState.FAILED);
        return;
    }

    component.connectionState = ConnectionState.RECONNECTING;
    component.reconnectAttempts++;

    var delay = Math.min(
        SSCConfig.initialReconnectDelay * Math.pow(SSCConfig.reconnectBackoffMultiplier, component.reconnectAttempts - 1),
        SSCConfig.maxReconnectDelay
    );

    if (debugMode) console.log('Scheduling reconnect attempt', component.reconnectAttempts, 'in', delay, 'ms');
    updateConnectionUI(component, ConnectionState.RECONNECTING);

    component.reconnectTimeout = setTimeout(function() {
        reconnectSSC(component);
    }, delay);
}

function reconnectSSC(component) {
    if (component.reconnectTimeout) {
        clearTimeout(component.reconnectTimeout);
        component.reconnectTimeout = null;
    }

    if (component.connection) {
        // Clean up old connection
        component.connection.onclose = null;
        component.connection.onerror = null;
        component.connection.onmessage = null;
        if (component.connection.readyState === WebSocket.OPEN) {
            component.connection.close();
        }
    }

    connectSSC(component);
}

function handleConnectionFailure(component) {
    component.connectionState = ConnectionState.FAILED;
    updateConnectionUI(component, ConnectionState.FAILED);
}

function updateConnectionUI(component, state) {
    // Remove any existing connection indicator
    var existingIndicator = component.querySelector('.ssc-connection-indicator');
    if (existingIndicator) {
        existingIndicator.remove();
    }

    // Only show indicator for non-connected states
    if (state === ConnectionState.CONNECTED) {
        return;
    }

    var indicator = document.createElement('div');
    indicator.className = 'ssc-connection-indicator';

    switch (state) {
        case ConnectionState.CONNECTING:
            indicator.style.cssText = 'background: #fef3c7; color: #92400e; padding: 8px 12px; border-radius: 4px; margin-bottom: 8px; font-size: 14px;';
            indicator.textContent = 'Connecting...';
            break;
        case ConnectionState.RECONNECTING:
            indicator.style.cssText = 'background: #fef3c7; color: #92400e; padding: 8px 12px; border-radius: 4px; margin-bottom: 8px; font-size: 14px;';
            indicator.textContent = 'Reconnecting... (attempt ' + component.reconnectAttempts + '/' + SSCConfig.maxReconnectAttempts + ')';
            break;
        case ConnectionState.DISCONNECTED:
            indicator.style.cssText = 'background: #fee2e2; color: #991b1b; padding: 8px 12px; border-radius: 4px; margin-bottom: 8px; font-size: 14px;';
            indicator.textContent = 'Disconnected';
            break;
        case ConnectionState.FAILED:
            indicator.style.cssText = 'background: #fee2e2; color: #991b1b; padding: 8px 12px; border-radius: 4px; margin-bottom: 8px; font-size: 14px;';
            indicator.innerHTML = 'Connection failed. <button onclick="this.parentNode.parentNode.reconnectAttempts = 0; reconnectSSC(this.parentNode.parentNode); this.parentNode.remove();" style="background: #991b1b; color: white; border: none; padding: 4px 8px; border-radius: 4px; cursor: pointer; margin-left: 8px;">Retry</button>';
            break;
    }

    component.insertBefore(indicator, component.firstChild);
}

// Unescapes a HTML-encoded string.
function htmlDecode(input) {
    var doc = new DOMParser().parseFromString(input, "text/html");
    return doc.documentElement.textContent;
}

function evaluatePatch(component, nodeOperations) {
    // Resolve all paths first, but we need to handle DOM mutations carefully
    // Since operations can change the DOM structure, we resolve paths just before each operation
    for (var i = 0; i < nodeOperations.length; i++) {
        var nodeOperation = nodeOperations[i];
        nodeOperation.domNode = resolvePathToNode(component, nodeOperation.path);

        if (debugMode) console.log(nodeOperation);
        evaluateNodeOperation(component, nodeOperation);
    }
}

function evaluateNodeOperation(component, nodeOperation) {
    switch (nodeOperation.type) {
        case 'CreateNode': return createNode(component, nodeOperation);
        case 'UpdateTextContent': return updateTextContent(component, nodeOperation);
        case 'UpdateNode': return updateNode(component, nodeOperation);
        case 'DeleteNode': return deleteNode(component, nodeOperation);
        case 'ReplaceNode': return replaceNode(component, nodeOperation);
        case 'UpdateComment': return updateComment(component, nodeOperation);
        case 'UpdatePreEscapedTextNode': return updatePreEscapedTextNode(component, nodeOperation);
        default:
            if (debugMode) console.log('Unhandled type', nodeOperation);
    }
}

function createNode(component, nodeOperation) {
    var newElement = htmlStringToDomNode(nodeOperation.html);
    var domNode = nodeOperation.domNode;
    if (domNode.nodeType == Node.TEXT_NODE) {
        domNode.replaceWith(newElement);
    } else {
        domNode.appendChild(newElement);
    }
}

function updateTextContent(component, nodeOperation) {
    var decodedContent = htmlDecode(nodeOperation.textContent)
    if (nodeOperation.domNode.textContent === decodedContent) {
        return;
    }
    nodeOperation.domNode.textContent = decodedContent;
}

function updateComment(component, nodeOperation) {
    // Comment nodes have nodeValue property for their content
    if (nodeOperation.domNode.nodeType === Node.COMMENT_NODE) {
        nodeOperation.domNode.nodeValue = nodeOperation.comment;
    }
}

function updatePreEscapedTextNode(component, nodeOperation) {
    // PreEscaped text nodes (like script/style content) should be updated as-is
    if (nodeOperation.domNode.nodeType === Node.TEXT_NODE) {
        nodeOperation.domNode.textContent = nodeOperation.textContent;
    }
}

function updateNode(component, nodeOperation) {
    for (var attributeOperation of nodeOperation.attributeOperations) {
        evaluateAttributeOperation(nodeOperation.domNode, attributeOperation);
    }
}

function deleteNode(component, nodeOperation) {
    nodeOperation.domNode.remove();
}

function replaceNode(component, nodeOperation) {
    var oldNode = nodeOperation.domNode;
    var parent = oldNode.parentNode;

    // Create a temporary container to parse the new HTML
    var template = document.createElement('template');
    template.innerHTML = nodeOperation.newNodeHtml;
    var newNode = template.content.firstChild;

    // Replace the old node with the new one
    if (parent && newNode) {
        parent.replaceChild(newNode, oldNode);

        // Initialize any nested SSC components in the new node
        if (newNode.nodeType === Node.ELEMENT_NODE) {
            // Check if the new node itself is an SSC component
            if (newNode.classList && newNode.classList.contains('ihp-ssc')) {
                initializeSSC(newNode);
            }
            // Check for nested SSC components
            var nestedComponents = newNode.querySelectorAll('.ihp-ssc');
            for (var i = 0; i < nestedComponents.length; i++) {
                initializeSSC(nestedComponents[i]);
            }
        }
    }
}

var inputTagnames = ["INPUT", "TEXTAREA"];

function evaluateAttributeOperation(domNode, op) {
    // HtmlDiff passes attribute values as raw values — that is, if the attribute value is a single quote mark it'll be
    // represented in HTML as "&quot;" and therefore we'll get "&quot;". We have to unescape those values before using
    // 'setAttribute' or 'x.value = y'.
    if (op.type === "UpdateAttribute" && op.attributeName == 'value' && inputTagnames.includes(domNode.tagName)) {
        domNode.value = htmlDecode(op.attributeValue);
    } else if (op.type === 'UpdateAttribute' || op.type === 'AddAttribute') {
        domNode.setAttribute(op.attributeName, htmlDecode(op.attributeValue));
    } else if (op.type === 'DeleteAttribute') {
        domNode.removeAttribute(op.attributeName);
    } else {
        throw new Error('Invalid op.type');
    }
}


function resolvePathToNode(component, path) {
    var current = component;

    // Traverse path from end to start (path is stored in reverse order)
    // Fixed: was `for (var i = path.length; i--; i > 0)` which had broken loop condition
    for (var i = path.length - 1; i >= 0; i--) {
        var childIndex = path[i];

        if (current.childNodes && current.childNodes[childIndex]) {
            current = current.childNodes[childIndex];
        } else {
            // Path is invalid - return current node and log warning
            if (debugMode) {
                console.warn('Invalid path segment:', childIndex, 'at depth', path.length - 1 - i, 'path:', path);
            }
            break;
        }
    }

    return current;
}

/* Returns the nearest element with a class `.ihp-ssc` */
var findActiveComponent = function (element) {
    // Check for null/undefined FIRST before accessing any properties
    if (!element) {
        throw new Error('Could not find nearest .ihp-ssc element');
    }

    if (element.classList && element.classList.contains('ihp-ssc')) {
        return element;
    }

    return findActiveComponent(element.parentNode);
};

function htmlStringToDomNode(string) {
    var template = document.createElement('template');
    template.innerHTML = string;
    return template.content;
}

// Expose functions globally
window.reconnectSSC = reconnectSSC;
window.initializeSSC = initializeSSC;
