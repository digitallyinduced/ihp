if (window.Turbolinks) {
    var oldAssignNewBody = Turbolinks.SnapshotRenderer.prototype.assignNewBody;

    Turbolinks.SnapshotRenderer.prototype.assignNewBody = function () {
        transitionToNewPage(this.newBody);
    };

    var oldPerformScroll = Turbolinks.Visit.prototype.performScroll;
    Turbolinks.Visit.prototype.performScroll = function () {
        this.scrollToTop = function () { };
        oldPerformScroll.call(this);
    };
}
