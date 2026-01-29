var socket = null;
var sessionId = null;
var autoRefreshPaused = false;

function autoRefreshView() {
    var metaTag = document.querySelector('meta[property="ihp-auto-refresh-id"]');

    if (!metaTag) {
        // Wait briefly for Turbo DOM morphing to complete
        setTimeout(function() {
            var retryMetaTag = document.querySelector('meta[property="ihp-auto-refresh-id"]');
            if (retryMetaTag) {
                processAutoRefresh(retryMetaTag);
            } else if (socket && !autoRefreshPaused) {
                socket.close();
            }
        }, 50);
        return;
    }

    processAutoRefresh(metaTag);
}

function processAutoRefresh(metaTag) {

    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    var socketHost = socketProtocol + "://" + window.location.hostname + ":" + document.location.port + '/AutoRefreshWSApp';
    if (socket && metaTag.content === sessionId) {
        // Socket is already running
        return;
    } else if (socket) {
        // Socket is running, but the page has changed
        socket.close();
        socket = new WebSocket(socketHost);
        sessionId = metaTag.content;
    } else {
        // First setup of socket
        socket = new WebSocket(socketHost);
        sessionId = metaTag.content;
    }

    autoRefreshPaused = false;

    socket.onopen = function (event) {
        socket.send(metaTag.content);
    };

    socket.onmessage = function (event) {
        if (autoRefreshPaused) {
            return;
        }

        var html = event.data;

        if (html.includes('<turbo-stream')) {
            // It's a Turbo Stream - let Turbo handle it
            document.body.insertAdjacentHTML('beforeend', html);
        } else {
            // It's full HTML - use morphPage for DOM morphing
            var parser = new DOMParser();
            var dom = parser.parseFromString(html, 'text/html');
            window.morphPage(dom);
        }
    };
}

/* Called by helpers.js when a form was just submitted and we're waiting for a response from the server */
window.pauseAutoRefresh = function () {
    autoRefreshPaused = true;
};

/* Called to resume auto-refresh after form submission completes */
window.resumeAutoRefresh = function () {
    autoRefreshPaused = false;
};

if (window.Turbo) {
    document.addEventListener('turbo:load', autoRefreshView);
} else {
    autoRefreshView();
}