
function initSchemaDesigner() {
    console.log('init');
    $('#new-column input[name="name"]').keyup(function () {
        const $input = $(this);

        // user_id => UUID
        if ($input.val().indexOf('_id') !== -1) {
            $('#typeSelector').val('UUID').trigger('change');
        }

        // projects_count => INT
        if ($input.val().indexOf('_count') !== -1) {
            $('#typeSelector').val('INT').trigger('change');
        }

        // created_at => TIMESTAMP WITH TIMEZONE
        if ($input.val().indexOf('_at') !== -1) {
            $('#typeSelector').val('TIMESTAMP WITH TIME ZONE').trigger('change');
        }
    });
    $('.select2').select2({ tags: true });
    $('.select2-simple').select2();
    $('#typeSelector').change(function () {
        switch (this.value) {
            case "UUID":
                $('#defaultSelector').empty()
                .append(new Option("no default", "NODEFAULT", true, true))
                .append(new Option("uuid_generate_v4()", 'uuid_generate_v4()', false, false))
                .append(new Option("''", "EMPTY", false, false))
                .append(new Option("null", "NULL", false, false))
                .trigger('change');
                break;
            case "TIMESTAMP WITH TIME ZONE":
                $('#defaultSelector').empty()
                .append(new Option("NOW()", 'NOW()', true, true))
                .append(new Option("no default", "NODEFAULT", false, false))
                .append(new Option("null", "NULL", false, false))
                .trigger('change');
                break;
            default:
                $('#defaultSelector').empty()
                .append(new Option("no default", "NODEFAULT", true, true))
                .append(new Option("''", "EMPTY", false, false))
                .append(new Option("null", "NULL", false, false))
                .trigger('change');
                break;
        }
    });
}

function initCodeEditor() {
    var editorEl = document.getElementById('editor');
    if (!editorEl) return;

    var editor;
    function initAce() {
        ace.require("ace/ext/language_tools");
        editor = ace.edit(editorEl);
        editor.setTheme("ace/theme/solarized_dark");
        editor.session.setMode("ace/mode/sql");
        editor.setShowPrintMargin(false);
        editor.setOptions({
            enableBasicAutocompletion: true,
            enableLiveAutocompletion: true
        });
        window.onbeforeunload = confirmExit;
        function confirmExit() {
            if (!editor.session.getUndoManager().isClean()) {
                return "You have unsaved changes. Do you want to leave the Editor?";
            }
        }
    }

    function initSaveButton() {
        var saveButton = document.getElementById("save-button");
        saveButton.disabled = true;
        saveButton.addEventListener("click", function saveSchema() {
            editor.session.getUndoManager().markClean()
            saveButton.disabled = editor.session.getUndoManager().isClean()
            var form = document.createElement('form');
            form.action = "http://localhost:8001/turbohaskell/SaveCode";
            form.method = 'POST';

            var methodInput = document.createElement('input');
            console.log(form.action);
            methodInput.type = 'hidden';
            methodInput.name = 'schemaSql';
            methodInput.value = editor.getValue();

            form.appendChild(methodInput);

            document.body.appendChild(form);
            form.submit();
        });

        editor.on("input", function() {
            saveButton.disabled = editor.session.getUndoManager().isClean()
        });
    }

    initAce();
    initSaveButton();
}

document.addEventListener('turbolinks:load', initSchemaDesigner);
document.addEventListener('turbolinks:load', initCodeEditor);
