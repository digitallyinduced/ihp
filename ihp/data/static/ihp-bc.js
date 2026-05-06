// IHP Backward Compatibility Stubs
// These functions were removed in the Turbo-native migration.
// This file prevents runtime errors for user code still referencing them.
// You can safely remove this script tag once your code no longer calls these functions.

if (!window.submitForm) {
    window.submitForm = function (form, possibleClickedButton) {
        console.warn('IHP: window.submitForm() is deprecated. Forms are now handled natively by Turbo. Use form.requestSubmit() instead.');
        form.requestSubmit(possibleClickedButton instanceof HTMLButtonElement ? possibleClickedButton : null);
    };
}

if (!window.transitionToNewPage) {
    window.transitionToNewPage = function (newHtml) {
        console.warn('IHP: window.transitionToNewPage() is deprecated. Use window.morphPage() instead.');
        if (window.morphPage) {
            window.morphPage(newHtml);
        }
    };
}
