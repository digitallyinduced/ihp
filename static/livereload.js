function ensureDevStyleLoaded() {
    if (document.getElementById('framework-dev-style')) {
        return;
    }

    var style = document.createElement('style');
    style.innerText = 'body.livereload-failure > * { filter: blur(2px); } body.livereload-failure > #livereload-warning { filter: blur(0) !important } #livereload-warning { padding-top: 33vh; background: rgba(255, 255, 255, 0.5); position: absolute; top: 0; width: 100vw; height: 100vw; left: 0; text-align: center; font-size: 48px; font-weight: bold; filter: blur(0.5) }';
    style.id = 'framework-dev-style';
    document.head.appendChild(style);
}

function refresh() {
    if (window.liveReloadPaused) {
        console.log('liveReloadPaused');
        return;
    }

    fetch(window.location.href, {credentials: 'include'})
        .then(response => { if (response.ok) return response.text(); else throw Error(response.statusText) })
        .catch(error => {
            ensureDevStyleLoaded();

            if (!document.getElementById('livereload-warning')) {
                document.body.classList.add('livereload-failure');
                var loadingMessage = document.createElement('div');
                loadingMessage.setAttribute('id', 'livereload-warning');
                loadingMessage.innerText = 'Waiting for server ...';
                document.body.appendChild(loadingMessage);
            }

            throw error;
        })
        .then(html => {
            var warning = document.getElementById('livereload-warning');
            if (warning) {
                warning.parentNode.removeChild(warning);
                document.body.classList.remove('livereload-failure');
            }

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
        });
}

function startReloadListener() {
    var notificationSocket = new WebSocket("ws://localhost:8002");
    notificationSocket.onmessage = function (event) {
        if (event.data === 'reload') {
            refresh();
        }
    }
}

if (window.liveReloadEnabled) {

} else {
    window.liveReloadEnabled = true;
    startReloadListener();
}
