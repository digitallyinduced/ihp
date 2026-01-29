var ihpLoadEvent = new Event('ihp:load');
var ihpUnloadEvent = new Event('ihp:unload');

window.addEventListener('beforeunload', function () {
    document.dispatchEvent(ihpUnloadEvent);
});

document.addEventListener('DOMContentLoaded', function () {
    initDelete();
    initBack();
    initToggle();
    initTime();
    initDatePicker();
    initFileUploadPreview();
    initTurboCompat();

    document.dispatchEvent(ihpLoadEvent);
});

document.addEventListener('turbo:load', function () {
    initDelete();
    initBack();
    initToggle();
    initTime();
    initFileUploadPreview();
    initTurboCompat();

    unsafeSetTimeout(function () {
        var elements = document.querySelectorAll('.js-scroll-into-view');
        for (var i in elements) {
            var element = elements[i];
            if (element && element.scrollIntoView)
                element.scrollIntoView({ behavior: 'smooth', block: 'start' });
        }
    }, 1);

    initDatePicker();

    document.dispatchEvent(ihpLoadEvent);
});

// Disable buttons on form submit (Turbo-native replacement for initDisableButtonsOnSubmit)
document.addEventListener('turbo:submit-start', function (event) {
    var form = event.target;

    // Disable all submit buttons in the form
    var buttons = form.getElementsByTagName('button');
    for (var j = 0; j < buttons.length; j++) {
        var button = buttons[j];
        if (button instanceof HTMLButtonElement) {
            button.setAttribute('disabled', 'disabled');
        }
    }

    // Dismiss alerts within the form
    var alerts = form.getElementsByClassName('alert');
    for (var j = 0; j < alerts.length; j++) {
        var alert = alerts[j];
        if (alert instanceof HTMLDivElement) {
            alert.classList.add('dismiss');
        }
    }

    // Pause auto-refresh during form submission
    if (window['pauseAutoRefresh']) {
        window.pauseAutoRefresh();
    }
});

document.addEventListener('turbo:submit-end', function (event) {
    // Re-enable buttons after submission completes
    var form = event.target;
    var buttons = form.getElementsByTagName('button');
    for (var j = 0; j < buttons.length; j++) {
        var button = buttons[j];
        if (button instanceof HTMLButtonElement) {
            button.removeAttribute('disabled');
        }
    }
});

function initTurboCompat() {
    document.querySelectorAll('form[data-disable-javascript-submission="true"]').forEach(function (form) {
        form.setAttribute('data-turbo', 'false');
    });
}

function initTime() {
    if (window.timeago) {
        window.timeago().render(document.querySelectorAll('.time-ago'));
    }

    document.querySelectorAll('.date-time').forEach(function (elem) {
        var date = new Date(elem.dateTime);
        elem.innerHTML =
            date.toLocaleDateString() +
            ', ' +
            date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
    });

    document.querySelectorAll('.date').forEach(function (elem) {
        var date = new Date(elem.dateTime);
        elem.innerHTML = date.toLocaleDateString();
    });

    document.querySelectorAll('.time').forEach(function (elem) {
        var date = new Date(elem.dateTime);
        elem.innerHTML = date.toLocaleTimeString(
            [],
            { hour: '2-digit', minute: '2-digit' }
        );
    });
}

function initDelete() {
    document.querySelectorAll('.js-delete').forEach(function (elem) {
        if (Boolean(elem.jsDeleteInitialized) === false) {
            elem.addEventListener('click', handleClick);
            elem.jsDeleteInitialized = true;
            // Prevent Turbo from prefetching DELETE links
            elem.setAttribute('data-turbo-prefetch', 'false');
        }
    });

    function validTargetElement(elem) {
        if (elem instanceof HTMLAnchorElement === false) {
            console.error('.js-delete only supports <a> elements', elem);
            return false;
        }

        if (elem.classList.contains('js-delete') === false) {
            // In case the `.js-delete` class was removed, and event listener is not detached. (after Turbo DOM update)
            return false;
        }

        return true;
    }

    function handleClick(event) {
        if (validTargetElement(event.currentTarget) === false) return;

        event.preventDefault();

        if (!event.currentTarget.classList.contains('js-delete-no-confirm')) {
            var confirmText =
                event.currentTarget.dataset.confirm ||
                'Are you sure you want to delete this?';
            if (!confirm(confirmText)) {
                return;
            }
        }

        var form = document.createElement('form');
        form.action = event.currentTarget.href;
        form.method = 'POST';

        var methodInput = document.createElement('input');
        methodInput.type = 'hidden';
        methodInput.name = '_method';
        methodInput.value = 'DELETE';

        form.appendChild(methodInput);

        document.body.appendChild(form);
        form.requestSubmit();
    }
}

function initBack() {
    var elements = document.getElementsByClassName('js-back');

    function handleClick(event) {
        event.preventDefault();
        event.target.setAttribute('disabled', 'disabled');
        event.target.classList.add('disabled');

        window.history.back();
    }

    for (var i in elements) {
        var element = elements[i];
        if (element instanceof HTMLButtonElement) {
            element.addEventListener('click', handleClick);
        } else if (element instanceof HTMLAnchorElement) {
            console.error(
                'js-back does not supports <a> elements, use a <button> instead',
                element
            );
        }
    }
}

function initToggle() {
    var elements = document.querySelectorAll('[data-toggle]');

    function handler() {
        var elements = document.querySelectorAll(
            this.getAttribute('data-toggle')
        );
        for (var i in elements) {
            var element = elements[i];
            if (!(element instanceof HTMLElement)) {
                return;
            }

            if (!this.checked) {
                element.setAttribute('disabled', 'disabled');
            } else {
                element.removeAttribute('disabled');
            }
        }
    }

    for (var i in elements) {
        var element = elements[i];
        if (!(element instanceof HTMLInputElement)) {
            continue;
        }

        element.addEventListener('change', handler);
        handler.call(element);
    }
}

function initFileUploadPreview() {
    var elements = document.querySelectorAll('input[type="file"]');

    function handler() {
        var input = this;
        if (input.files && input.files[0]) {
            var reader = new FileReader();
            reader.onload = function (e) {
                document
                    .querySelector(input.getAttribute('data-preview'))
                    .setAttribute('src', e.target.result);
            };
            reader.readAsDataURL(input.files[0]);
        }
    }

    for (var i in elements) {
        var element = elements[i];
        if (!(element instanceof HTMLInputElement)) continue;

        if (element.getAttribute('data-preview'))
            element.addEventListener('change', handler.bind(element));
    }
}

function initDatePicker() {
    if (!('flatpickr' in window)) {
        return;
    }

    document.querySelectorAll("input[type='date']").forEach(el => {
        flatpickr(el, {
            ...(el.dataset.altFormat ? {} : { altFormat: 'd.m.y' }),
            ...(el.dataset.altInput ? {} : { altInput: true }),
        });
    });

    document.querySelectorAll("input[type='datetime-local']").forEach(el => {
        flatpickr(el, {
            ...(el.dataset.enableTime ? {} : { enableTime: true }),
            ...(el.dataset.time_24hr ? {} : { time_24hr: true }),
            ...(el.dataset.dateFormat ? {} : { dateFormat: 'Z' }),
            ...(el.dataset.altFormat ? {} : { altFormat: 'd.m.y, H:i' }),
            ...(el.dataset.altInput ? {} : { altInput: true }),
        });
    });
}

window.morphPage = function (newHtml) {
    document.dispatchEvent(ihpUnloadEvent);

    // Build a turbo-stream to morph the body
    var newBody = newHtml.tagName === 'BODY' ? newHtml : newHtml.body;
    if (!newBody) return;

    // Ensure body has an id for targeting
    if (!document.body.id) document.body.id = 'ihp-body';

    // Handle modal preservation
    var isModalOpen = document.body.classList.contains('modal-open');
    var newBodyHasModal = newBody.classList.contains('modal-open');

    var targetId, content;
    if (isModalOpen && !newBodyHasModal) {
        targetId = 'main-row';
        var newMainRow = newBody.querySelector('#main-row');
        content = newMainRow ? newMainRow.innerHTML : '';
    } else {
        targetId = document.body.id;
        content = newBody.innerHTML;
    }

    var streamHTML = '<turbo-stream action="update" target="' + targetId + '" method="morph"><template>' + content + '</template></turbo-stream>';
    Turbo.renderStreamMessage(streamHTML);

    window.clearAllIntervals();
    window.clearAllTimeouts();

    // Update auto-refresh meta tag
    var ihpAutoRefreshId = newHtml.head && newHtml.head.querySelector('meta[property="ihp-auto-refresh-id"]');
    if (ihpAutoRefreshId) {
        var prev = document.head.querySelector('meta[property="ihp-auto-refresh-id"]');
        if (prev) prev.remove();
        document.head.appendChild(ihpAutoRefreshId);
    }

    var event = new CustomEvent('turbo:load', {});
    document.dispatchEvent(event);
};

if (!('allIntervals' in window)) {
    window.allIntervals = [];
    window.allTimeouts = [];

    window.unsafeSetInterval = window.setInterval;
    window.unsafeSetTimeout = window.setTimeout;

    window.setInterval = function () {
        var id = unsafeSetInterval.apply(window, arguments);
        window.allIntervals.push(id);
        return id;
    };

    window.setTimeout = function () {
        var id = unsafeSetTimeout.apply(window, arguments);
        window.allTimeouts.push(id);
        return id;
    };

    window.clearAllIntervals = function () {
        for (var i = 0; i < window.allIntervals.length; i++) {
            clearInterval(window.allIntervals[i]);
        }

        var oldLength = window.allIntervals.length;
        window.allIntervals = new Array(oldLength);
    };

    window.clearAllTimeouts = function () {
        for (var i = 0; i < window.allTimeouts.length; i++) {
            clearTimeout(window.allTimeouts[i]);
        }

        var oldLength = window.allTimeouts.length;
        window.allTimeouts = new Array(oldLength);
    };
}
