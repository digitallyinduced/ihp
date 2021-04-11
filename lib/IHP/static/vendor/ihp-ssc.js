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
        var newElement = document.createElement('div');
        newElement.innerHTML = event.data;

        morphdom(component, newElement, { childrenOnly: true });
    };
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