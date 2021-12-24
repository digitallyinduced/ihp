
function initSchemaDesigner() {
    $('#nameInput, #colName').on('input', function() {
        var reservedKeywords = [ "_", "as", "case", "class", "data", "default", "deriving", "do", "else", "hiding", "if"
        , "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "qualified", "then"
        , "type", "where", "forall", "mdo", "family", "role", "pattern", "static", "group", "by", "using", "foreign", "export"
        , "label", "dynamic", "safe", "interruptible", "unsafe", "stdcall", "ccall", "capi", "prim", "javascript", "rec", "proc" ];
        if (reservedKeywords.includes(this.value)) {
            this.classList = "form-control is-invalid"
            var warning = document.createElement("div");
            warning.innerHTML =
		"'" + this.value + "' is a reserved keyword in Haskell. "
		+ "Consider renaming it to '"
		+ (this.dataset.tableNameSingular || 'something') + "_" + this.value
		+ "'.";
            warning.classList = "invalid-feedback";
            warning.setAttribute("id", "warningText");
            this.parentNode.insertBefore(warning, this.nextSibling);
        } else {
            $('#warningText').remove();
            this.classList = "form-control"
        }
    });
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

        // date_ => DATE
        if ($input.val().indexOf('date_') === 0) {
            $('#typeSelector').val('DATE').trigger('change');
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
                    .append(new Option("no default", "", false, false))
                    .trigger('change');
                } else {
                    $('#defaultSelector').empty()
                    .append(new Option("no default", "", true, true))
                    .trigger('change');
                }
                break;
            case "TIMESTAMP WITH TIME ZONE":
                $('#defaultSelector').empty()
                .append(new Option("NOW()", 'NOW()', true, true))
                .append(new Option("no default", "", false, false))
                .trigger('change');
                break;
            case "TIMESTAMP WITHOUT TIME ZONE":
                $('#defaultSelector').empty()
                .append(new Option("NOW()", 'NOW()', true, true))
                .append(new Option("no default", "", false, false))
                .trigger('change');
                break;
            case "TEXT":
                $('#defaultSelector').empty()
                .append(new Option("no default", "", true, true))
                .append(new Option("\"\"", "''", false, false))
                .trigger('change');
                break;
            case "INT":
                $('#defaultSelector').empty()
                .append(new Option("no default", "", true, true))
                .append(new Option("0", "0", false, false))
                .trigger('change');
                break;
            case "BOOLEAN":
                $('#defaultSelector').empty()
                .append(new Option("false", "false", true, true))
                .append(new Option("true", "true", false, false))
                .append(new Option("no default", "", false, false))
                .trigger('change');
                break;
            default:
                $('#defaultSelector').empty()
                .append(new Option("no default", "", true, true))
                .trigger('change');
                break;
        }
    });
    $('#allowNull').change(function() {
        if ($('#allowNull').is(":checked")) {
            if(0 == $("#defaultSelector option:contains('NULL')").length)
                $('#defaultSelector').append(new Option("NULL", "NULL", true, true));
        } else {
            $("#defaultSelector option[value='NULL']").remove();
        }
    });
    $('#isArray').change(function() {
        if ($('#isArray').is(":checked")) {
            if(0 == $("#defaultSelector option:contains(\"'{}'\")").length)
                $('#defaultSelector').append(new Option("'{}'", "'{}'", true, true));
        } else {
            $("#defaultSelector option[value=\"'{}'\"]").remove();
        }
    });
    $("#colName").on('input', function () {
        $('.ref').hide();
        $('.ref > input').prop('checked', false);
        $('label[data-attribute="' + $("#colName").val() +'"]').show();
        $('label[data-attribute="' + $("#colName").val() +'"] > input').prop('checked', true);
        $('input[name="referenceTable"]').val($('label[style="font-size: 12px;"]').data("table"));
    });

    initEnumValueNameSnakeCaseCheck();
    closeModalOnEscape();
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
    var editor = ace.edit(editorEl);
    var existingValue = editor.getValue();
    editor.destroy();
    function initAce() {
        ace.require("ace/ext/language_tools");
        editor = ace.edit(editorEl);
        editor.setValue(existingValue, 1);
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
            window.location = "/ShowQuery?query=" + editor.getValue();
        });
    }
    initAce();
    editor.focus();
    editor.navigateFileEnd();
}


function fillField(id, value, isBoolean) {
    if (isBoolean) {
        setCheckboxSqlMode(id, true)
        var inputField = document.getElementById(id + "-alt");
    } else {
        setSqlMode(id, true);
        var inputField = document.getElementById(id + "-input");
    }
    var sqlModeBox = document.getElementById(id + "-sqlbox");
    inputField.value = value;
    sqlModeBox.checked = true;
}

function sqlModeCheckbox(id, checkbox, isBoolean) {
    if (isBoolean) {
        setCheckboxSqlMode(id, checkbox.checked)
    } else {
        setSqlMode(id, checkbox.checked);
    }
}

function setSqlMode(id, sqlMode) {
    var inputField = document.getElementById(id + "-input");
    if (sqlMode) {
        inputField.className = "form-control text-monospace text-secondary bg-light"
    } else {
        inputField.className = "form-control";
    }
}

function stopSqlModeOnInput(id) {
    var sqlModeBox = document.getElementById(id + "-sqlbox");
    sqlModeBox.checked = false;
    setSqlMode(id, false);
}

function setCheckboxSqlMode(id, sqlMode) {
    var inputField = document.getElementById(id + "-input");
    var hiddenField = document.getElementById(id + "-hidden");
    var checkBoxContainer = document.getElementById(id + "-boxcontainer");
    var altField = document.getElementById(id + "-alt");
    if (sqlMode) {
        inputField.className = "d-none";
        inputField.name = id + "-inactive";
        hiddenField.name = id + "-inactive";
        altField.className = "form-control text-monospace text-secondary bg-light";
        altField.name = id;
        checkBoxContainer.className = "d-none";

    } else {
        inputField.className = "";
        inputField.name = id;
        hiddenField.name = id;
        altField.className = "d-none";
        altField.name = id + "-inactive";
        checkBoxContainer.className = "form-control";
    }
}

/** Shows a warning if a new enum value is going to be added that is not in snake case */
function initEnumValueNameSnakeCaseCheck() {
    const input = document.querySelector('input[name="enumValueName"]');
    if (!input) {
        return;
    }

    input.onkeyup = event => {
        const enumValueName = event.target.value;
        const isCamelCase = /[A-Z]/.test(enumValueName)

        if (isCamelCase) {
            input.classList.add('is-invalid');
        } else {
            input.classList.remove('is-invalid');
        }
    }
}

function closeModalOnEscape() {
    document.addEventListener('keydown', event => {
        const escKey = 27;
        if (event.keyCode != escKey) {
            return;
        }

        const modalBackdrop = document.getElementById('modal-backdrop');
        modalBackdrop?.click();
    })
}

// Triggers a call to window.onbeforeunload if needed
// https://github.com/digitallyinduced/ihp/issues/954
function checkBeforeUnload() {
    if (window.onbeforeunload) {
        var r = window.onbeforeunload();
        if (!r) {
            return;
        }

        if (confirm(r)) {
            event.preventDefault();
            event.stopPropagation();
        } else {
            window.onbeforeunload = null;
        }
    }
}
