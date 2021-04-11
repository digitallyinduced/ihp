window.callServerAction = function (action, payload) {
    var component = findActiveComponent(event.target || document.activeElement);
    var message = JSON.stringify({ action, ...payload });
    component.connection.send(message)
}

for (var component of document.querySelectorAll('.ihp-ssc')) {
    initalizeSSC(component)
}

function initalizeSSC(component) {
    component.connection = new WebSocket(component.dataset.url);

    component.connection.onopen = function (event) {
        console.log('Connected');
    };
    
    component.connection.onclose = function (event) {
        component.innerText = 'This component crashed :(';
    };

    component.connection.onmessage = function (event) {
        var payload = JSON.parse(event.data);
        evaluatePatch(component, payload);
        console.log(payload);
        //var newElement = document.createElement('div');
        //newElement.innerHTML = event.data;

        //morphdom(component, newElement, { childrenOnly: true });
    };
}

function evaluatePatch(component, nodeOperations) {
    for (var nodeOperation of nodeOperations) {
        nodeOperation.domNode = resolvePathToNode(component, nodeOperation.path);
    }
    for (var nodeOperation of nodeOperations) {
        console.log(nodeOperation);
        evaluateNodeOperation(component, nodeOperation);
    }
}

function evaluateNodeOperation(component, nodeOperation) {
    switch (nodeOperation.type) {
        case 'CreateNode': return createNode(component, nodeOperation);
        case 'UpdateTextContent': return updateTextContent(component, nodeOperation);
        case 'UpdateNode': return updateNode(component, nodeOperation);
        case 'DeleteNode': return deleteNode(component, nodeOperation);
        default:
            console.log('Unhandled type', nodeOperation);
    }
}

function createNode(component, nodeOperation) {
    var newElement = htmlStringToDomNode(nodeOperation.html);
    nodeOperation.domNode.appendChild(newElement);
}

function updateTextContent(component, nodeOperation) {
    nodeOperation.domNode.textContent = nodeOperation.textContent;
}

function updateNode(component, nodeOperation) {
    for (var attributeOperation of nodeOperation.attributeOperations) {
        evaluateAttributeOperation(nodeOperation.domNode, attributeOperation);
    }
}

function deleteNode(component, nodeOperation) {
    nodeOperation.domNode.remove();
}

function evaluateAttributeOperation(domNode, op) {
    if (op.type === 'UpdateAttribute' || op.type === 'AddAttribute') {
        domNode.setAttribute(op.attributeName, op.attributeValue);
    } else if (op.type === 'DeleteAttribute') {
        domNode.removeAttribute(op.attributeName);
    } else {
        throw new Error('Invalid op.type');
    }
}

function resolvePathToNode(component, path) {
    var current = component;

    //console.log('Resolving path: ', path);
    //console.log('current = ', current);
    for (var i = path.length; i--; i > 0) {
        //console.log('i = ', i);
        var currentPathElement = path[i];

        if (current.childNodes[currentPathElement])
            current = current.childNodes[currentPathElement];
        //console.log('current = childNodes[j]', current);
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