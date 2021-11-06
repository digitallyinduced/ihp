function initPolicyEditor() {
    console.log('initPolicyEditor');

    const editorElements = document.querySelectorAll('.edit-policy .sql-expression');
    ace.require("ace/ext/language_tools");

    editorElements.forEach(editorEl => {
        const autocompleteSuggestions = editorEl.dataset.autocompleteSuggestions.split(',');

        console.log('SEL', 'input[type="hidden"][name="' + editorEl.name + '"]');
        const hiddenField = editorEl.form.querySelector('input[type="hidden"][name="' + editorEl.name + '"]');

        const editor = ace.edit(editorEl);

        ace.require("ace/ext/language_tools");
        editor.setTheme("ace/theme/solarized_dark");
        editor.session.setMode("ace/mode/sql");
        editor.setOptions({
            enableBasicAutocompletion: true,
            enableLiveAutocompletion: true,
            showGutter: false,
            fontSize: 14,
            highlightActiveLine: false
        });
        editor.renderer.setPadding(8);
        editor.renderer.setScrollMargin(8);
        editor.completers.push({
            getCompletions: function (editor, session, pos, prefix, callback) {
                var completions = autocompleteSuggestions.map(suggestion => ({
                    value: suggestion,
                    meta: "Column"
                }));

                completions.push({
                    value: 'ihp_user_id()',
                    meta: 'UUID or NULL'
                });
                
                callback(null, completions);
            }
        });

        editor.getSession().on("change", () => {
            hiddenField.setAttribute('value', editor.getSession().getValue());
        });
    });
}

document.addEventListener('turbolinks:load', initPolicyEditor);