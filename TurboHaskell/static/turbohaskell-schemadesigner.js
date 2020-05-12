
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

document.addEventListener('turbolinks:load', initSchemaDesigner);
