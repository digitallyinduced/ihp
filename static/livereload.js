let h = require('virtual-dom/h');
let diff = require('virtual-dom/diff');
let patch = require('virtual-dom/patch');
let virtualize = require('vdom-virtualize');

var VNode = require('virtual-dom/vnode/vnode');
var VText = require('virtual-dom/vnode/vtext');

var convertHTML = require('html-to-vdom')({
    VNode: VNode,
    VText: VText
}).bind(null, {getVNodeKey: function (attributes) { return attributes.id || attributes.style; }})


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
            return convertHTML(dom.body.parentElement.outerHTML);
        })
        .then(newDocument => {
            if (!newDocument) {
                return;
            }

            let newDom = newDocument;
            let html = document.body.parentElement.outerHTML;
            let currentDom = convertHTML(
                html
            );
            let patches = diff(currentDom, newDom);
            patch(document.body.parentElement, patches);

            var event = new CustomEvent('turbolinks:load', {});
            document.dispatchEvent(event);
        })
}

if (window.liveReloadEnabled) {
    return;
}
window.liveReloadEnabled = true;


document.addEventListener('DOMContentLoaded', function () {
    var interval = parseInt(document.getElementById('livereload-script').getAttribute('data-interval') || 1000);
    setInterval(refresh, interval);
});
