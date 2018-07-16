var oldAssignNewBody = Turbolinks.SnapshotRenderer.prototype.assignNewBody;

var locked = false;
Turbolinks.SnapshotRenderer.prototype.assignNewBody = function () {
    if (locked) {
        return;
    }

    morphdom(document.body, this.newBody, {
        childrenOnly: true,
    });
    locked = true;

    setTimeout(function () {
        locked = false;
    }, 100);
}
