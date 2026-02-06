(function () {
  if (window.__ihpAutoRefreshInitialized) {
    return;
  }
  window.__ihpAutoRefreshInitialized = true;
  var autoRefreshPaused = false;
  var autoRefreshTargetCounter = 0;
  var autoRefreshSessions = {};
  var inflightRequests = 0;

  var morphdomOptions = {
    getNodeKey: function (el) {
      var key = el.id;
      if (el.id) {
        key = el.id;
      } else if (el.form && el.name) {
        key = el.name + '_' + el.form.action;
      } else if (el instanceof HTMLFormElement) {
        key = 'form#' + el.action;
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
    },
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
    var existing = document.head.querySelectorAll(
      'meta[property="ihp-auto-refresh-id"]'
    );
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
      if (
        meta.parentNode &&
        meta.parentNode.tagName === 'HEAD' &&
        meta.ownerDocument === document
      ) {
        return;
      }

      var clone = meta.cloneNode(true);
      if (!clone.getAttribute('data-ihp-auto-refresh-target')) {
        var inferredTarget = inferTargetSelector(meta, fallbackTarget);
        if (inferredTarget) {
          clone.setAttribute('data-ihp-auto-refresh-target', inferredTarget);
        }
      }
      if (meta.parentNode && meta.ownerDocument !== document) {
        meta.parentNode.removeChild(meta);
      }
      replaceAutoRefreshMeta(clone);
    });
  }

  function readAutoRefreshConfigs() {
    var metas = document.head.querySelectorAll(
      'meta[property="ihp-auto-refresh-id"]'
    );

    if (!metas || metas.length === 0) {
      harvestAutoRefreshMetaFromNode(document);
      metas = document.head.querySelectorAll(
        'meta[property="ihp-auto-refresh-id"]'
      );
    }

    if (!metas || metas.length === 0) {
      return [];
    }

    metas = Array.prototype.filter.call(metas, function (meta) {
      var target = getMetaTarget(meta);
      if (!target) {
        return true;
      }
      try {
        if (document.querySelector(target)) {
          return true;
        }
      } catch (error) {}
      console.log('[auto-refresh] remove stale meta', target);
      if (meta.parentNode) {
        meta.parentNode.removeChild(meta);
      }
      return false;
    });

    if (!metas || metas.length === 0) {
      return [];
    }

    var configs = [];
    var seen = {};
    Array.prototype.forEach.call(metas, function (meta) {
      var target = getMetaTarget(meta);
      var config = {
        sessionId: meta.content,
        target: target,
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
    return (
      socketProtocol +
      '://' +
      window.location.hostname +
      ':' +
      document.location.port +
      '/AutoRefreshWSApp'
    );
  }

  function openAutoRefreshSession(config, key) {
    var session = {
      sessionId: config.sessionId,
      targetSelector: config.target || null,
      socket: null,
      pendingHtml: null,
    };

    var socket = new WebSocket(socketHost());
    session.socket = socket;
    autoRefreshPaused = inflightRequests > 0;

    socket.onopen = function () {
      socket.send(session.sessionId);
    };

    socket.onmessage = function (event) {
      handleIncomingHtml(event.data, session);
    };

    socket.onerror = function () {
      console.error(
        '[auto-refresh] ws error',
        session.targetSelector || 'body'
      );
    };

    socket.onclose = function (event) {
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

  function resumePendingRefreshes() {
    var sessions = Object.keys(autoRefreshSessions).map(function (key) {
      return autoRefreshSessions[key];
    });
    sessions.forEach(function (session) {
      if (session && session.pendingHtml) {
        var html = session.pendingHtml;
        session.pendingHtml = null;
        handleIncomingHtml(html, session);
      }
    });
  }

  function handleIncomingHtml(html, session) {
    if (autoRefreshPaused) {
      if (session) {
        session.pendingHtml = html;
      }
      return;
    }

    if (session) {
      session.pendingHtml = null;
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
      if (window.htmx) {
        htmx.process(target);
      }
    } else {
      morphdom(document.body, dom.body, morphdomOptions);
      if (window.htmx) {
        htmx.process(document.body);
      }
    }

    if (typeof window.clearAllIntervals === 'function') {
      window.clearAllIntervals();
    }
    if (typeof window.clearAllTimeouts === 'function') {
      window.clearAllTimeouts();
    }

    var loadEvent = new CustomEvent('turbolinks:load', {});
    document.dispatchEvent(loadEvent);
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
    function isFullPageSwap(event) {
      var target =
        event && event.detail && event.detail.target
          ? event.detail.target
          : event.target;
      var boosted = event && event.detail && event.detail.boosted;
      var responseText =
        event && event.detail && event.detail.xhr
          ? event.detail.xhr.responseText || ''
          : '';
      return (
        boosted ||
        target === document.body ||
        (target && target.tagName === 'BODY') ||
        (target && target.tagName === 'HTML') ||
        responseText.indexOf('<html') !== -1 ||
        responseText.indexOf('<head') !== -1
      );
    }

    document.addEventListener('htmx:beforeRequest', function (event) {
      if (event && event.detail && event.detail.boosted) {
        console.log('[auto-refresh] htmx boosted request, closing');
        closeAllSessions();
      }
      inflightRequests += 1;
      autoRefreshPaused = true;
    });

    document.addEventListener('htmx:beforeSwap', function (event) {
      if (isFullPageSwap(event)) {
        console.log('[auto-refresh] htmx full-page swap, closing');
        closeAllSessions();
      }
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
      ensureTargetHasId(target);


      if (event && event.detail && event.detail.xhr) {
        var responseText = event.detail.xhr.responseText;
        if (responseText) {
          var parser = new DOMParser();
          var dom = parser.parseFromString(responseText, 'text/html');
          harvestAutoRefreshMetaFromNode(dom, target);
        } else {
          harvestAutoRefreshMetaFromNode(target, target);
        }
      } else {
        harvestAutoRefreshMetaFromNode(target, target);
      }
      autoRefreshView();
    });
  }
})();
