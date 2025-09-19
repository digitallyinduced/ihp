var socket = null;
var sessionId = null;
var autoRefreshTargetSelector = null;
var autoRefreshPaused = false;

var morphdomOptions = {
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
    onBeforeElChildrenUpdated: function (fromEl, toEl) {
        if (fromEl.tagName === 'TEXTAREA' || fromEl.tagName === 'INPUT') {
            toEl.checked = fromEl.checked;
            toEl.value = fromEl.value;
        } else if (fromEl.tagName === 'OPTION') {
            toEl.selected = fromEl.selected;
        }
    }
};

function cloneMorphdomOptions(extraOptions) {
    var clone = {};
    var key;
    for (key in morphdomOptions) {
        if (Object.prototype.hasOwnProperty.call(morphdomOptions, key)) {
            clone[key] = morphdomOptions[key];
        }
    }
    for (key in extraOptions) {
        if (Object.prototype.hasOwnProperty.call(extraOptions, key)) {
            clone[key] = extraOptions[key];
        }
    }
    return clone;
}

function replaceAutoRefreshMeta(meta) {
    if (!meta) {
        return;
    }

    var existing = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(existing, function (node) {
        if (node.parentNode) {
            node.parentNode.removeChild(node);
        }
    });
    document.head.appendChild(meta);
}

function harvestAutoRefreshMetaFromNode(node) {
    if (!node || !node.querySelectorAll) {
        return;
    }

    var metas = node.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(metas, function (meta) {
        if (meta.parentNode && meta.parentNode.tagName === 'HEAD') {
            return;
        }

        var clone = meta.cloneNode(true);
        meta.parentNode.removeChild(meta);
        replaceAutoRefreshMeta(clone);
    });
}

function readAutoRefreshConfig() {
    var meta = document.head.querySelector('meta[property="ihp-auto-refresh-id"]');

    if (!meta) {
        harvestAutoRefreshMetaFromNode(document);
        meta = document.head.querySelector('meta[property="ihp-auto-refresh-id"]');
    }

    if (!meta) {
        return null;
    }

    var target = meta.getAttribute('data-ihp-auto-refresh-target');
    return {
        sessionId: meta.content,
        target: target && target.length > 0 ? target : null
    };
}

function ensureSocketClosed() {
    if (socket) {
        socket.close();
        socket = null;
    }
}

function autoRefreshView() {
    var config = readAutoRefreshConfig();

    if (!config) {
        ensureSocketClosed();
        sessionId = null;
        autoRefreshTargetSelector = null;
        return;
    }

    autoRefreshTargetSelector = config.target || null;

    if (socket && config.sessionId === sessionId) {
        return;
    }

    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    var socketHost =
        socketProtocol + '://' + window.location.hostname + ':' + document.location.port + '/AutoRefreshWSApp';

    ensureSocketClosed();

    socket = new WebSocket(socketHost);
    sessionId = config.sessionId;
    autoRefreshPaused = false;

    socket.onopen = function () {
        socket.send(sessionId);
    };

    socket.onmessage = function (event) {
        handleIncomingHtml(event.data);
    };
}

function handleIncomingHtml(html) {
    if (autoRefreshPaused) {
        return;
    }

    var parser = new DOMParser();
    var dom = parser.parseFromString(html, 'text/html');

    harvestAutoRefreshMetaFromNode(dom);

    var config = readAutoRefreshConfig();
    if (config) {
        autoRefreshTargetSelector = config.target || autoRefreshTargetSelector;
        if (config.sessionId !== sessionId) {
            sessionId = config.sessionId;
        }
    }

    var targetSelector = autoRefreshTargetSelector;
    if (targetSelector) {
        var target = document.querySelector(targetSelector);
        if (!target) {
            return;
        }
        var newTarget = dom.querySelector(targetSelector);
        if (newTarget) {
            morphdom(target, newTarget, morphdomOptions);
        } else {
            morphdom(target, dom.body, cloneMorphdomOptions({ childrenOnly: true }));
        }
    } else {
        morphdom(document.body, dom.body, morphdomOptions);
    }

    window.clearAllIntervals();
    window.clearAllTimeouts();

    var loadEvent = new CustomEvent('turbolinks:load', {});
    document.dispatchEvent(loadEvent);
}

/* Called by helpers.js when a form was just submitted and we're waiting for a response from the server */
window.pauseAutoRefresh = function () {
    autoRefreshPaused = true;
};

if (window.Turbolinks) {
    document.addEventListener('turbolinks:load', autoRefreshView);
} else {
    autoRefreshView();
}

if (window.htmx) {
    document.addEventListener('htmx:afterSwap', function (event) {
        var target = event && event.detail && event.detail.target ? event.detail.target : event.target;
        harvestAutoRefreshMetaFromNode(target);
        autoRefreshView();
    });
}
