var locked = false;

if (window.Turbolinks) {
    var oldAssignNewBody = Turbolinks.SnapshotRenderer.prototype.assignNewBody;

    Turbolinks.SnapshotRenderer.prototype.assignNewBody = function () {
        transitionToNewPage(this.newBody);
    };
}

var debugMorphdom = false;
window.transitionToNewPage = function (newBody) {
    if (locked) {
        return;
    }

    var keepScrollPosition = true;
    var scrollPosition = null;

    if (keepScrollPosition)
        scrollPosition = $(window).scrollTop();;

    console.log(scrollPosition);

    var isModalOpen = document.body.classList.contains('modal-open');
    morphdom(document.body, newBody, {
        childrenOnly: true,
        onBeforeElUpdated: function (from, to) {

            if (newBody.classList.contains('modal-open') && from.id === 'main-row') {
                return false;
            } else if (isModalOpen && from.id === 'main-row') {
                return false;
            } else if (from.parentNode && from.parentNode.tagName === 'svg') {
                return false;
            }

            if (debugMorphdom)
                console.log('onBeforeElUpdated', from, to)
        },
        onBeforeElChildrenUpdated: function (from, to) {
            if (debugMorphdom)
                console.log('onBeforeElChildrenUpdated', from, to);
        },
        onBeforeNodeDiscarded: function (el) {
            if (el.classList.contains('animate-delete')) {
                el.style.height = window.getComputedStyle(el).height;
                // el.style.height = window.getComputedStyle(el).height;

                setTimeout(function (el) {
                    el.classList.add('delete');
                }, 0, el);
                setTimeout(function (el) {
                    el.parentNode.removeChild(el);
                }, 300, el);
                return false;
            }

            if (debugMorphdom)
                console.log('onBeforeNodeDiscarded', el);
        },
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
        onBeforeNodeAdded: function (el) {
            if (debugMorphdom)
                console.log('onBeforeNodeAdded', el);
        }
    });
    locked = true;

    setTimeout(function () {
        locked = false;
    }, 100);
}
