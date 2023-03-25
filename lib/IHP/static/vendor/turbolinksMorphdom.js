if (window.Turbolinks) {
    var oldAssignNewBody = Turbolinks.SnapshotRenderer.prototype.assignNewBody;

    Turbolinks.SnapshotRenderer.prototype.assignNewBody = function () {
        transitionToNewPage(this.newBody);
    };

    var oldPerformScroll = Turbolinks.Visit.prototype.performScroll;
    Turbolinks.Visit.prototype.performScroll = function () {
        var scrollToTopEnabled = window['TURBOLINKS_SCROLL_TO_TOP'] === true;
        if (!scrollToTopEnabled)
            this.scrollToTop = function () { };

        oldPerformScroll.call(this);
    };
}
