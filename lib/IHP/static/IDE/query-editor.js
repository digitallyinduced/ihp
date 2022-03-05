$(document).on('ready turbolinks:load', function () {
    initQueryEditor();
});

function initQueryEditor() {
    var form = document.querySelector('form.sql-repl');

    if (!form) return;

    if (window.queryEditor) {
        window.queryEditor.destroy();
        window.queryEditor = null;
    }

    var editorEl = document.querySelector('.query-editor');
    var input = form.querySelector('input[name="query"]');

    if ($(input).val() === "") {
        $(input).val(localStorage.getItem('query-editor') || '');
    }

    var editor = ace.edit(editorEl);
    window.queryEditor = editor;

    const defaultValue = $(input).val();

    ace.require("ace/ext/language_tools");
    editor = ace.edit(editorEl);
    editor.setValue(defaultValue, 1);
    editor.setTheme("ace/theme/solarized_dark");
    editor.session.setMode("ace/mode/sql");
    editor.setShowPrintMargin(false);
    editor.setOptions({
        enableBasicAutocompletion: true,
        enableLiveAutocompletion: true,
        showGutter: false,
        maxLines: 20,
        minLines: 5
    });
    editor.setHighlightActiveLine(false);
    editor.commands.bindKey("Shift-Enter|Command-Enter", function() {
        submitForm(form);
    });
    editor.getSession().on('change', function() {
        const value = editor.getSession().getValue();
        $(input).val(value);
        localStorage.setItem('query-editor', value);
    });
    editor.focus();
    editor.navigateFileEnd();
}