function initGeneratorPreview() {
    console.log('initGeneratorPreview');

    const editorElements = document.querySelectorAll('.generator-action .file-content');
    ace.require("ace/ext/language_tools");

    editorElements.forEach(editorEl => {
        const editor = ace.edit(editorEl);
        // editor.setTheme("ace/theme/solarized_dark");
        editor.session.setMode("ace/mode/haskell");
        editor.setShowPrintMargin(false);
        editor.setHighlightActiveLine(false);
        editor.setReadOnly(true);
        editor.setOptions({
            showLineNumbers: false,
            showGutter: false,
            wrap: true,
            maxLines: 12,
            fontSize: 10
        });
    });

    const nameInput = document.querySelector('.new-controller input[name="name"]');
    var currentTimeout = null;
    if (nameInput) {
        nameInput.onkeyup = () => {
            var actions = document.querySelector('.generator-actions');
            if (actions) actions.remove();
            if (currentTimeout) clearTimeout(currentTimeout);
            currentTimeout = setTimeout(() => window.submitForm(nameInput.form), 200);
        }
    }
}

document.addEventListener('turbolinks:load', initGeneratorPreview);