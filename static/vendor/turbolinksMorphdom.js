var oldAssignNewBody = Turbolinks.SnapshotRenderer.prototype.assignNewBody;

var locked = false;
Turbolinks.SnapshotRenderer.prototype.assignNewBody = function () {
    if (locked) {
        return;
    }

    var keepScrollPosition = true;
    var scrollPosition = null;

    if (keepScrollPosition)
        scrollPosition = $(window).scrollTop();;

    console.log(scrollPosition);

    var newBody = this.newBody;

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

            console.log('onBeforeElUpdated', from, to)
        },
        onBeforeElChildrenUpdated: function (from, to) {
            console.log('onBeforeElChildrenUpdated', from, to);
        },
        onBeforeNodeDiscarded: function (el) {
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
            console.log('onBeforeNodeAdded', el);
        }
    });
    locked = true;

    setTimeout(function () {
        locked = false;
    }, 100);
}
