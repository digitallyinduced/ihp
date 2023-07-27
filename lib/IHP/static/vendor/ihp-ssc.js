var debugMode = document.currentScript.dataset.debugMode || false;

window.callServerAction = function (action, payload) {
    var component = findActiveComponent(event.target || document.activeElement);
    var message = JSON.stringify({ action, ...payload });
    component.connection.send(message)
}


if (window.Turbolinks) {
    document.addEventListener('turbolinks:load', initalizeAllSSCComponents);
} else {
    initalizeAllSSCComponents();
}

function initalizeAllSSCComponents() {
    for (var component of document.querySelectorAll('.ihp-ssc')) {
        if (debugMode) console.log('init', component)
        initalizeSSC(component);
    }
}

function initalizeSSC(component) {
    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    var socketHost = socketProtocol + "://" + window.location.hostname + ":" + document.location.port;

    component.connection = new WebSocket(socketHost + component.dataset.path);

    component.connection.onopen = function (event) {
        if (debugMode) console.log('Connected');

        // Send Initial State
        component.connection.send(component.dataset.initialState)
    };

    component.connection.onclose = function (event) {
        component.innerText = 'This component crashed :(';
    };

    component.connection.onmessage = function (event) {
        var payload = JSON.parse(event.data);
        evaluatePatch(component, payload);
        if (debugMode) console.log(payload);
        //var newElement = document.createElement('div');
        //newElement.innerHTML = event.data;

        //morphdom(component, newElement, { childrenOnly: true });
    };
}

// Unescapes a HTML-encoded string.
function htmlDecode(input) {
    var doc = new DOMParser().parseFromString(input, "text/html");
    return doc.documentElement.textContent;
}

function evaluatePatch(component, nodeOperations) {
    for (var nodeOperation of nodeOperations) {
        nodeOperation.domNode = resolvePathToNode(component, nodeOperation.path);
    }
    for (var nodeOperation of nodeOperations) {
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

function updateNode(component, nodeOperation) {
    for (var attributeOperation of nodeOperation.attributeOperations) {
        evaluateAttributeOperation(nodeOperation.domNode, attributeOperation);
    }
}

function deleteNode(component, nodeOperation) {
    nodeOperation.domNode.remove();
}

function replaceNode(component, nodeOperation) {
    nodeOperation.domNode.outerHTML = nodeOperation.newNodeHtml;
    if (debugMode) console.log('A#######', nodeOperation.domNode);
    if (nodeOperation.domNode.type == Node.ELEMENT_NODE) {
        for (var component of nodeOperation.domNode.querySelectorAll('.ihp-ssc')) {
            initalizeSSC(component);
        }
    }
}

const inputTagnames = ["INPUT", "TEXTAREA"];

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

    for (var i = path.length; i--; i > 0) {
        var currentPathElement = path[i];

        if (current.childNodes[currentPathElement])
            current = current.childNodes[currentPathElement];
    }

    return current;
}

/* Returns the nearest element with a class `.ihp-ssc` */
var findActiveComponent = function (element) {
    if (element.classList.contains('ihp-ssc')) {
        return element;
    }
    if (!element) {
        throw new Error('Could not find nearest .ihp-ssc element');
    }

    return findActiveComponent(element.parentNode);
};

function htmlStringToDomNode(string) {
    var template = document.createElement('template');
    template.innerHTML = string;
    return template.content;
}
