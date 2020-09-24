var socket = null;
var sessionId = null;
function autoRefreshView() {
    var metaTag = document.querySelector('meta[property="ihp-auto-refresh-id"]');
    if (!metaTag) {
        if (socket) {
            console.log('Closing socket');
            socket.close();
        }
        return;
    }

    if (socket && metaTag.content === sessionId) {
        // Socket is already running
        return;
    } else if (socket) {
        // Socket is running, but the page has changed
        socket.close();
        socket = new WebSocket("ws://localhost:" + document.location.port);
        sessionId = metaTag.content;
    } else {
        // First setup of socket
        socket = new WebSocket("ws://localhost:" + document.location.port);
        sessionId = metaTag.content;
    }

    socket.onopen = function (event) {
        socket.send(metaTag.content);
    };

    socket.onmessage = function (event) {
        var html = event.data;
        var parser = new DOMParser();
        var dom = parser.parseFromString(html, 'text/html');
        morphdom(document.body, dom.body, {
            getNodeKey: function (el) {

                var key = el.id;
                if (el.id) {
                    key = el.id;
                } else if (el.form && el.name) {
                    key = el.name + "_" + el.form.action;
                } else if (el instanceof HTMLFormElement) {
                    key = "form#" + el.action;
                } else if (el instanceof HTMLScriptElement) {
                    key = el.src;
                }
                return key;
            },
            onBeforeElChildrenUpdated: function(fromEl, toEl) {
                if (fromEl.tagName === 'TEXTAREA' || fromEl.tagName === 'INPUT') {
                    toEl.checked = fromEl.checked;
                    toEl.value = fromEl.value;
                } else if (fromEl.tagName === 'OPTION') {
                    toEl.selected = fromEl.selected;
                }
            }
        });

        window.clearAllIntervals();
        window.clearAllTimeouts();
        
        var event = new CustomEvent('turbolinks:load', {});
        document.dispatchEvent(event);
    };
}

if (window.Turbolinks) {
    document.addEventListener('turbolinks:load', autoRefreshView);
} else {
    autoRefreshView();
}