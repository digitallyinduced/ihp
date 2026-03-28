(function () {
  if (window.__ihpAutoRefreshInitialized) {
    return;
  }
  window.__ihpAutoRefreshInitialized = true;

  var autoRefreshSessions = {};
  var autoRefreshPaused = false;
  var inflightRequests = 0;

  var morphdomOptions = {
    getNodeKey: function (el) {
      if (el.id) {
        return el.id;
      }
      if (el.form && el.name) {
        return el.name + '_' + el.form.action;
      }
      if (el instanceof HTMLFormElement) {
        return 'form#' + el.action;
      }
      if (el instanceof HTMLScriptElement) {
        return el.src;
      }
      return null;
    },
    onBeforeElChildrenUpdated: function (fromEl, toEl) {
      if (fromEl.tagName === 'TEXTAREA' || fromEl.tagName === 'INPUT') {
        toEl.checked = fromEl.checked;
        toEl.value = fromEl.value;
      } else if (fromEl.tagName === 'OPTION') {
        toEl.selected = fromEl.selected;
      }
    },
  };

  function childrenOnlyMorphdomOptions() {
    return {
      getNodeKey: morphdomOptions.getNodeKey,
      onBeforeElChildrenUpdated: morphdomOptions.onBeforeElChildrenUpdated,
      childrenOnly: true,
    };
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

  function replaceAutoRefreshMeta(meta, fallbackTarget) {
    if (!meta) {
      return;
    }

    if (
      !meta.getAttribute('data-ihp-auto-refresh-target') &&
      fallbackTarget &&
      fallbackTarget.id
    ) {
      meta.setAttribute('data-ihp-auto-refresh-target', '#' + fallbackTarget.id);
    }

    var metaTarget = getMetaTarget(meta);
    var existing = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(existing, function (node) {
      if (getMetaTarget(node) === metaTarget && node.parentNode) {
        node.parentNode.removeChild(node);
      }
    });

    document.head.appendChild(meta);
  }

  function harvestAutoRefreshMeta(root, fallbackTarget) {
    if (!root || !root.querySelectorAll) {
      return;
    }

    var metas = root.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    Array.prototype.forEach.call(metas, function (meta) {
      if (meta.parentNode === document.head) {
        return;
      }

      replaceAutoRefreshMeta(meta.cloneNode(true), fallbackTarget);

      if (meta.parentNode) {
        meta.parentNode.removeChild(meta);
      }
    });
  }

  function readAutoRefreshConfigs() {
    var metas = document.head.querySelectorAll('meta[property="ihp-auto-refresh-id"]');
    if (!metas || metas.length === 0) {
      return [];
    }

    var configs = [];
    var seen = {};

    Array.prototype.forEach.call(metas, function (meta) {
      if (!meta.content) {
        return;
      }

      var config = {
        sessionId: meta.content,
        target: getMetaTarget(meta),
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

  function socketHost() {
    var socketProtocol = location.protocol === 'https:' ? 'wss' : 'ws';
    return (
      socketProtocol +
      '://' +
      window.location.hostname +
      ':' +
      document.location.port +
      '/AutoRefreshWSApp'
    );
  }

  function closeSession(key) {
    var session = autoRefreshSessions[key];
    if (!session) {
      return;
    }

    if (session.socket) {
      session.isClosing = true;

      // Closing a socket while CONNECTING causes noisy browser errors
      // ("WebSocket is closed before the connection is established").
      // Let it finish opening and close it in onopen instead.
      if (session.socket.readyState === WebSocket.OPEN) {
        session.socket.close();
      }
    }
    delete autoRefreshSessions[key];
  }

  function closeAllSessions() {
    Object.keys(autoRefreshSessions).forEach(function (key) {
      closeSession(key);
    });
  }

  function applyHtmlUpdate(dom, session) {
    if (session.targetSelector) {
      var target = document.querySelector(session.targetSelector);
      if (!target) {
        return;
      }

      var newTarget = dom.querySelector(session.targetSelector);
      if (newTarget) {
        morphdom(target, newTarget, morphdomOptions);
      } else {
        morphdom(target, dom.body, childrenOnlyMorphdomOptions());
      }

      if (window.htmx) {
        htmx.process(target);
      }
      return;
    }

    morphdom(document.body, dom.body, morphdomOptions);
    if (window.htmx) {
      htmx.process(document.body);
    }
  }

  function afterMorphdomUpdate() {
    if (typeof window.clearAllIntervals === 'function') {
      window.clearAllIntervals();
    }
    if (typeof window.clearAllTimeouts === 'function') {
      window.clearAllTimeouts();
    }

    document.dispatchEvent(new CustomEvent('turbolinks:load', {}));
  }

  function handleIncomingHtml(html, session) {
    if (autoRefreshPaused) {
      session.pendingHtml = html;
      return;
    }

    session.pendingHtml = null;

    var dom = new DOMParser().parseFromString(html, 'text/html');
    var fallbackTarget = session.targetSelector
      ? document.querySelector(session.targetSelector)
      : null;

    harvestAutoRefreshMeta(dom, fallbackTarget);
    autoRefreshView();

    applyHtmlUpdate(dom, session);
    afterMorphdomUpdate();
  }

  function openAutoRefreshSession(config, key) {
    var session = {
      sessionId: config.sessionId,
      targetSelector: config.target || null,
      socket: null,
      pendingHtml: null,
      isClosing: false,
    };

    session.socket = new WebSocket(socketHost());

    session.socket.onopen = function () {
      if (session.isClosing || autoRefreshSessions[key] !== session) {
        session.socket.close();
        return;
      }
      session.socket.send(session.sessionId);
    };

    session.socket.onmessage = function (event) {
      if (session.isClosing || autoRefreshSessions[key] !== session) {
        return;
      }
      handleIncomingHtml(event.data, session);
    };

    session.socket.onclose = function () {
      if (autoRefreshSessions[key] === session) {
        delete autoRefreshSessions[key];
      }
    };

    return session;
  }

  function autoRefreshView() {
    var configs = readAutoRefreshConfigs();

    if (!configs || configs.length === 0) {
      closeAllSessions();
      return;
    }

    var nextKeys = {};

    configs.forEach(function (config) {
      var key = getSessionKey(config);
      nextKeys[key] = true;

      var existing = autoRefreshSessions[key];
      if (existing && existing.sessionId === config.sessionId) {
        return;
      }

      if (existing) {
        closeSession(key);
      }

      autoRefreshSessions[key] = openAutoRefreshSession(config, key);
    });

    Object.keys(autoRefreshSessions).forEach(function (key) {
      if (!nextKeys[key]) {
        closeSession(key);
      }
    });
  }

  function resumePendingRefreshes() {
    Object.keys(autoRefreshSessions).forEach(function (key) {
      var session = autoRefreshSessions[key];
      if (!session || !session.pendingHtml) {
        return;
      }

      var html = session.pendingHtml;
      session.pendingHtml = null;
      handleIncomingHtml(html, session);
    });
  }

  window.pauseAutoRefresh = function () {
    autoRefreshPaused = true;
  };

  if (window.Turbolinks) {
    document.addEventListener('turbolinks:load', autoRefreshView);
  } else {
    autoRefreshView();
  }

  if (window.htmx) {
    document.addEventListener('htmx:beforeRequest', function () {
      inflightRequests += 1;
      autoRefreshPaused = true;
    });

    document.addEventListener('htmx:afterRequest', function () {
      inflightRequests = Math.max(0, inflightRequests - 1);
      autoRefreshPaused = inflightRequests > 0;
      if (!autoRefreshPaused) {
        resumePendingRefreshes();
      }
    });

    document.addEventListener('htmx:afterSwap', function (event) {
      var target =
        event && event.detail && event.detail.target
          ? event.detail.target
          : event.target;

      harvestAutoRefreshMeta(target, target);
      autoRefreshView();
    });
  }
})();
