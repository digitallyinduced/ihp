let lastHtml = null;
function refresh() {
    fetch(window.location.href, {credentials: 'include'})
        .then(response => response.text())
        .then(html => {
            if (html === lastHtml) {
                return null;
            }
            if (lastHtml === null) {
                lastHtml = html;
                return;
            } else {
                lastHtml = html;
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
                    console.log('getNodeKey', key, el);
                    return key;
                },
                onElUpdated: function () {
                    var event = new CustomEvent('turbolinks:load', {});
                    document.dispatchEvent(event);
                },
                onBeforeElChildrenUpdated: function(fromEl, toEl) {
                    console.log('x');
                    if (fromEl.tagName === 'TEXTAREA' || fromEl.tagName === 'INPUT') {
                        toEl.checked = fromEl.checked;
                        toEl.value = fromEl.value;
                    } else if (fromEl.tagName === 'OPTION') {
                        toEl.selected = fromEl.selected;
                    }
                }
            });
        })
}

if (window.liveReloadEnabled) {

} else {
    window.liveReloadEnabled = true;
    document.addEventListener('DOMContentLoaded', function () {
        var interval = parseInt(document.getElementById('livereload-script').getAttribute('data-interval') || 1000);
        setInterval(refresh, interval);
    });
}
