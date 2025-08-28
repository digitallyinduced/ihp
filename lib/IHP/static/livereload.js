function refresh() {
    if (window.liveReloadPaused) {
        console.log('liveReloadPaused');
        return;
    }

    function delay(ms) { return new Promise((resolve, reject) => { setTimeout(resolve, ms); }); }

    function fetchWithRetries(url, options, n) {
        return fetch(url, options).catch(function (error) {
            if (n === 1) throw error;
            return delay(10).then(() => fetchWithRetries(url, options, n - 1));
        })
    }

    fetchWithRetries(window.location.href, {credentials: 'include'}, 50)
        .then(response => {
            if (response.ok || (response.status === 500 && response.statusText == 'Internal Server Error'))
                return response.text();
            else
                throw Error(response.statusText);
        }).catch(error => {
            console.log('Live Reload Failed', error);

            throw error;
        })
        .then(html => {
            var parser = new DOMParser();
            var dom = parser.parseFromString(html, 'text/html');

            // Use Turbo's built-in morphing for live reload
            // https://github.com/digitallyinduced/ihp/issues/998
            if (window.Turbo && window.Turbo.renderPage) {
                // Let Turbo handle the live reload with morphing
                window.Turbo.renderPage({
                    snapshot: { html: dom.documentElement.outerHTML },
                    isPreview: false
                });
            } else {
                // Fallback: use Turbo Stream refresh action for live reload
                var refreshStream = '<turbo-stream action="refresh"></turbo-stream>';
                document.body.insertAdjacentHTML('beforeend', refreshStream);
            }

            window.clearAllIntervals();
            window.clearAllTimeouts();

            var event = new CustomEvent('turbo:load', {});
            document.dispatchEvent(event);
        });
}

function refreshAssets() {
    var stylesheets = document.querySelectorAll('link[rel="stylesheet"]');
    for (const stylesheet of stylesheets) {
        if (!stylesheet.dataset.originalHref)
            stylesheet.dataset.originalHref = stylesheet.href
        stylesheet.href = stylesheet.dataset.originalHref + '?refresh=' + (+new Date());
    }
}

function getLiveReloadWSUrl() {
    var script = document.getElementById('livereload-script');
    if (script && script.dataset.ws) {
        return script.dataset.ws;
    }

    var port = (parseInt(document.location.port, 10) || 8000) + 1;
    return "ws://localhost:" + port;
}

function startReloadListener() {
    var notificationSocket = new WebSocket(getLiveReloadWSUrl());
    var wasOpened = false;
    notificationSocket.onopen = function (event) { wasOpened = true; }
    notificationSocket.onmessage = function (event) {
        wasOpened = true;
        if (event.data === 'reload') {
            refresh();
        } else if (event.data === 'reload_assets')
            refreshAssets();
    }
    notificationSocket.onclose = function (event) { if (wasOpened) window.location.reload(); }
}

if (window.liveReloadEnabled) {

} else {
    window.liveReloadEnabled = true;
    startReloadListener();
}
