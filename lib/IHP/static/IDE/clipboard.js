function initClipboard() {
    new ClipboardJS('[data-clipboard-text]');
}
$(document).on("ready turbolinks:load", initClipboard);
