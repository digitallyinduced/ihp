document.addEventListener('turbolinks:load', function () {
    var navHelp = document.getElementById('nav-help');
    if (!navHelp) return;

    new bootstrap.Popover(navHelp, {
        container: '#content',
        html: true,
        content: document.getElementById('help-content').innerHTML,
    });

    navHelp.addEventListener('click', function (event) {
        event.preventDefault();
    });
});