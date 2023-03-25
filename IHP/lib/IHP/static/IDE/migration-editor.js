$(document).on('ready turbolinks:load', function () {
    initMigrationEditor();
});

function initMigrationEditor() {
    var aceContainer = document.getElementById('migration_sqlStatements_ace');
    var textarea = document.getElementById('migration_sqlStatements');
    if (!aceContainer) {
        return;
    }

    if (window.migrationEditor) {
        window.migrationEditor.destroy();
    }


    ace.require("ace/ext/language_tools");
    var editor = ace.edit(aceContainer);

    window.migrationEditor = editor;

    editor.getSession().setValue($(textarea).val());
    editor.setTheme("ace/theme/solarized_dark");
    editor.session.setMode("ace/mode/sql");
    editor.setShowPrintMargin(false);
    editor.setOptions({
        enableBasicAutocompletion: true,
        enableLiveAutocompletion: true,
        showGutter: false,
        maxLines: 80,
        minLines: 5
    });

    editor.setHighlightActiveLine(false);
    editor.commands.bindKey("Shift-Enter|Command-Enter", function() {
        submitForm(textarea.form);
    });

    
    editor.getSession().on('change', function(){
        $(textarea).val(editor.getSession().getValue());
    });

    textarea.classList.add('d-none')

    editor.focus();
    editor.navigateFileEnd();
}