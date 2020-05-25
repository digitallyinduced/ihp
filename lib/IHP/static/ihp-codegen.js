function initGeneratorPreview() {
    console.log('initGeneratorPreview');

    const editorElements = document.querySelectorAll('.generator-action .file-content');
    ace.require("ace/ext/language_tools");

    editorElements.forEach(editorEl => {
        const editor = ace.edit(editorEl);
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
}

document.addEventListener('turbolinks:load', initGeneratorPreview);