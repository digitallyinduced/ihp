$(function () {
    $('#nav-help').popover({
        container: '#content',
        html: true,
        content: document.getElementById('help-content').innerHTML,
    });

    document.getElementById('nav-help').addEventListener('click', event => {
        event.preventDefault();
    })
})