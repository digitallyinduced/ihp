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

        // Use Turbo Streams for auto-refresh with intelligent DOM updates
        var html = event.data;
        
        // Check if it's a Turbo Stream or full HTML
        if (html.includes('<turbo-stream')) {
            // It's a Turbo Stream - let Turbo handle it
            document.body.insertAdjacentHTML('beforeend', html);
        } else {
            // It's full HTML - send a refresh stream to trigger Turbo morphing
            var refreshStream = '<turbo-stream action="refresh"></turbo-stream>';
            document.body.insertAdjacentHTML('beforeend', refreshStream);
        }

        window.clearAllIntervals();
        window.clearAllTimeouts();
        
        // Update event to use Turbo
        var event = new CustomEvent('turbo:load', {});
        document.dispatchEvent(event);
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