var autoRefreshSessions = {};
var autoRefreshPaused = false;
var autoRefreshTargetCounter = 0;

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

function escapeCssIdentifier(value) {
    if (window.CSS && CSS.escape) {
        return CSS.escape(value);
    }
    return value.replace(/([^\w-])/g, '\\$1');
}

function inferTargetSelector(meta, fallbackTarget) {
    if (fallbackTarget && fallbackTarget.id) {
        return '#' + escapeCssIdentifier(fallbackTarget.id);
    }
    if (meta && meta.parentElement && meta.parentElement.id) {
        return '#' + escapeCssIdentifier(meta.parentElement.id);
    }
    return null;
}

function ensureTargetHasId(target) {
    if (!target || target.id) {
        return;
    }

    var id;
    do {
        autoRefreshTargetCounter += 1;
        id = 'ihp-auto-refresh-target-' + autoRefreshTargetCounter;
    } while (document.getElementById(id));

    target.id = id;
}

function getMetaTarget(meta) {
    if (!meta) {
        return null;
    }
    var target = meta.getAttribute('data-ihp-auto-refresh-target');
    return target && target.length > 0 ? target : null;
}

function getSessionKey(config) {
    return config.target ? 'target:' + config.target : 'body';
}

function replaceAutoRefreshMeta(meta) {
    if (!meta) {
        return;
    }

    var metaTarget = getMetaTarget(meta);
    var existing = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(existing, function (node) {
        var nodeTarget = getMetaTarget(node);
        if (nodeTarget === metaTarget && node.parentNode) {
            node.parentNode.removeChild(node);
        }
    });
    document.head.appendChild(meta);
}

function harvestAutoRefreshMetaFromNode(node, fallbackTarget) {
    if (!node || !node.querySelectorAll) {
        return;
    }

    var metas = node.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(metas, function (meta) {
        if (meta.parentNode && meta.parentNode.tagName === 'HEAD') {
            return;
        }

        var clone = meta.cloneNode(true);
        if (!clone.getAttribute('data-ihp-auto-refresh-target')) {
            var inferredTarget = inferTargetSelector(meta, fallbackTarget);
            if (inferredTarget) {
                clone.setAttribute('data-ihp-auto-refresh-target', inferredTarget);
            }
        }
        meta.parentNode.removeChild(meta);
        replaceAutoRefreshMeta(clone);
    });
}

function readAutoRefreshConfigs() {
    var metas = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');

    if (!metas || metas.length === 0) {
        harvestAutoRefreshMetaFromNode(document);
        metas = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    }

    if (!metas || metas.length === 0) {
        return [];
    }

    var configs = [];
    var seen = {};
    Array.prototype.forEach.call(metas, function (meta) {
        var target = getMetaTarget(meta);
        var config = {
            sessionId: meta.content,
            target: target
        };
        var key = getSessionKey(config);
        if (seen[key]) {
            return;
        }
        seen[key] = true;
        configs.push(config);
    });
    return configs;
}

function ensureSocketClosed(session) {
    if (session && session.socket) {
        session.socket.close();
        session.socket = null;
    }
}

function socketHost() {
    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    return socketProtocol + '://' + window.location.hostname + ':' + document.location.port + '/AutoRefreshWSApp';
}

function openAutoRefreshSession(config, key) {
    var session = {
        sessionId: config.sessionId,
        targetSelector: config.target || null,
        socket: null
    };

    var socket = new WebSocket(socketHost());
    session.socket = socket;
    autoRefreshPaused = false;

    socket.onopen = function () {
        socket.send(session.sessionId);
    };

    socket.onmessage = function (event) {
        handleIncomingHtml(event.data, session);
    };

    socket.onclose = function () {
        if (autoRefreshSessions[key] === session) {
            delete autoRefreshSessions[key];
        }
    };

    return session;
}

function closeAllSessions() {
    Object.keys(autoRefreshSessions).forEach(function (key) {
        ensureSocketClosed(autoRefreshSessions[key]);
        delete autoRefreshSessions[key];
    });
}

function autoRefreshView() {
    var configs = readAutoRefreshConfigs();

    if (!configs || configs.length === 0) {
        closeAllSessions();
        return;
    }

    var nextKeys = {};
    configs.forEach(function (config) {
        if (!config.sessionId) {
            return;
        }
        var key = getSessionKey(config);
        nextKeys[key] = true;
        var existing = autoRefreshSessions[key];
        if (existing && existing.sessionId === config.sessionId) {
            return;
        }
        if (existing) {
            ensureSocketClosed(existing);
        }
        autoRefreshSessions[key] = openAutoRefreshSession(config, key);
    });

    Object.keys(autoRefreshSessions).forEach(function (key) {
        if (!nextKeys[key]) {
            ensureSocketClosed(autoRefreshSessions[key]);
            delete autoRefreshSessions[key];
        }
    });
}

function handleIncomingHtml(html, session) {
    if (autoRefreshPaused) {
        return;
    }

    var parser = new DOMParser();
    var dom = parser.parseFromString(html, 'text/html');

    var fallbackTarget = null;
    if (session && session.targetSelector) {
        fallbackTarget = document.querySelector(session.targetSelector);
    }
    harvestAutoRefreshMetaFromNode(dom, fallbackTarget);
    autoRefreshView();

    var targetSelector = session ? session.targetSelector : null;
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
        ensureTargetHasId(target);
        harvestAutoRefreshMetaFromNode(target, target);
        autoRefreshView();
    });
}
