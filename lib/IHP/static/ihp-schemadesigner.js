
function initSchemaDesigner() {
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

        // is_ => BOOLEAN
        if ($input.val().indexOf('is_') === 0) {
            $('#typeSelector').val('BOOLEAN').trigger('change');
        }
    });
    $('.select2').select2({ tags: true });
    $('.select2-simple').select2();
    $('#typeSelector').change(function () {
        switch (this.value) {
            case "UUID":
                if ($('label[data-attribute="' + $("#colName").val() +'"]').length == 0) {
                    $('#defaultSelector').empty()
                    .append(new Option("uuid_generate_v4()", 'uuid_generate_v4()', true, true))
                    .append(new Option("no default", "NODEFAULT", false, false))
                    .trigger('change');
                } else {
                    $('#defaultSelector').empty()
                    .append(new Option("no default", "NODEFAULT", true, true))
                    .trigger('change');
                }
                break;
            case "TIMESTAMP WITH TIME ZONE":
                $('#defaultSelector').empty()
                .append(new Option("NOW()", 'NOW()', true, true))
                .append(new Option("no default", "NODEFAULT", false, false))
                .trigger('change');
                break;
            case "TEXT":
                $('#defaultSelector').empty()
                .append(new Option("''", "EMPTY", true, true))
                .append(new Option("no default", "NODEFAULT", false, false))
                .trigger('change');
                break;
            case "BOOLEAN":
                $('#defaultSelector').empty()
                .append(new Option("false", "false", true, true))
                .append(new Option("true", "true", false, false))
                .append(new Option("no default", "NODEFAULT", false, false))
                .trigger('change');
                break;
            default:
                $('#defaultSelector').empty()
                .append(new Option("no default", "NODEFAULT", true, true))
                .trigger('change');
                break;
        }
    });
    $('#allowNull').change(function() {
        if ($('#allowNull').is(":checked")) {
            $('#defaultSelector').append(new Option("null", "NULL", true, true));
        } else {
            $("#defaultSelector option[value='NULL']").remove();
        }
    });
    $("#colName").on('input', function () {
        $('.ref').hide();
        $('.ref > input').prop('checked', false);
        $('label[data-attribute="' + $("#colName").val() +'"]').show();
        $('label[data-attribute="' + $("#colName").val() +'"] > input').prop('checked', true);
        $('input[name="referenceTable"]').val($('label[style="font-size: 12px;"]').data("table"));
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
            form.action = document.location.origin + "/SaveCode";
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

function initTooltip() {
    $('[data-toggle="tooltip"]').tooltip('dispose').tooltip({ container: 'body'});
}

document.addEventListener('turbolinks:load', initSchemaDesigner);
document.addEventListener('turbolinks:load', initCodeEditor);
document.addEventListener('turbolinks:load', initQueryAce);
document.addEventListener('turbolinks:load', initTooltip);

function initQueryAce() {
    var editorEl = document.getElementById('queryInput');
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
            enableLiveAutocompletion: true,
            showGutter: false,
            maxLines: 1
        });
        editor.commands.bindKey("Enter|Shift-Enter", function() {
            window.location = "/ihp/ShowQuery?query=" + editor.getValue();
        });
    }
    initAce();
    editor.focus();
    editor.navigateFileEnd();
}