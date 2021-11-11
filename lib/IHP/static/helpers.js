var ihpLoadEvent = new Event('ihp:load');
var ihpUnloadEvent = new Event('ihp:unload');

window.addEventListener('beforeunload', function () {
    document.dispatchEvent(ihpUnloadEvent);
});

document.addEventListener('DOMContentLoaded', function () {
    initDelete();
    initDisableButtonsOnSubmit();
    initBack();
    initToggle();
    initTime();
    initDatePicker();
    initFileUploadPreview();

    document.dispatchEvent(ihpLoadEvent);
});

document.addEventListener('turbolinks:load', function () {
    initDelete();
    initBack();
    initToggle();
    initTime();
    initFileUploadPreview();

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
        }
    });

    function validTargetElement(elem) {
        if (elem instanceof HTMLAnchorElement === false) {
            console.error('.js-delete only supports <a> elements', elem);
            return false;
        }

        if (elem.classList.contains('js-delete') === false) {
            // In case the `.js-delete` class was removed, and event listener is not detached. (after morphdom DOM patch)
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
        window.submitForm(form);
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

function initDisableButtonsOnSubmit() {
    if (window.initDisableButtonsOnSubmitRun) {
        return;
    }
    window.initDisableButtonsOnSubmitRun = true;

    var lastClicked = null;
    document.addEventListener('submit', function (event) {
        event.preventDefault();

        var form = event.target;
        window.submitForm(form, lastClicked);
    });

    document.addEventListener('mouseup', function (event) {
        lastClicked = event.target;
    });
}

window.submitForm = function (form, possibleClickedButton) {
    if (form.dataset && (form.dataset.disableJavascriptSubmission === 'true')) {
        form.submit();
        return;
    }

    // We cannot use `form.action` here because there could be a <input name="action"/>
    // See https://github.com/digitallyinduced/ihp/issues/1203
    var formAction = form.getAttribute('action');
    var formMethod = form.getAttribute('method');

    var request = new XMLHttpRequest();
    request.responseType = 'document';
    request.overrideMimeType('text/html');
    request.onload = function () {
        console.info('AJAX', this.status, this.responseURL);
        if (request.readyState !== request.DONE) {
            return;
        }
        if (request.status !== 200 && request.status !== 280) {
            console.error(
                'Something went wrong, status code: ' + request.status
            );
        }

        if (window.Turbolinks) {
            var snapshot = new Turbolinks.Snapshot(
                new Turbolinks.HeadDetails(request.response.head),
                request.response.body
            );
            transitionToNewPage(request.response);
            Turbolinks.clearCache();
            if (request.responseURL !== formAction) {
                history.pushState({ turbolinks: true }, '', request.responseURL);
            };
            var turbolinkLoadEvent = new CustomEvent('turbolinks:load');
            document.dispatchEvent(turbolinkLoadEvent);
        } else {
            window.liveReloadPaused = true;

            if (request.responseURL !== formAction) {
                history.pushState({}, '', request.responseURL);
                window.onpopstate = function (event) {
                    window.location.reload();
                };
            }

            transitionToNewPage(request.response);
            var turbolinkLoadEvent = new CustomEvent('turbolinks:load');
            document.dispatchEvent(turbolinkLoadEvent);

            var reenableLiveReload = function () {
                window.liveReloadPaused = false;

                document.removeEventListener(
                    'turbolinks:load',
                    reenableLiveReload
                );
            };

            document.addEventListener('turbolinks:load', reenableLiveReload);
        }
    };

    var submit = document.activeElement;
    if (!submit || submit instanceof HTMLBodyElement) {
        submit = possibleClickedButton;
    }

    var formData = new FormData(form);
    console.log(form, formData, submit);

    if (
        (submit instanceof HTMLInputElement ||
            (submit instanceof HTMLButtonElement &&
                submit.getAttribute('type') == 'submit')) &&
        submit.form == form
    ) {
        var submitName = submit.getAttribute('name');
        if (submitName !== null)
            formData.set(submitName, submit.value);
    }

    var hasFileInputs = form.querySelector('input[type="file"]');
    if (hasFileInputs) {
        request.open(formMethod, formAction, true);
        request.send(formData);
    } else {
        var parameters = [];

        if (formMethod.toUpperCase() === 'GET') {
            var url = new URL(formAction);
            for (var pair of formData.entries()) {
                url.searchParams.set(pair[0], pair[1]);
            }

            request.open(formMethod, url.toString(), true);
        } else {
            for (var pair of formData.entries()) {
                parameters.push(
                    encodeURIComponent(pair[0]) + '=' + encodeURIComponent(pair[1])
                );
            }

            request.open(formMethod, formAction, true);
        }

        request.setRequestHeader(
            'Content-Type',
            'application/x-www-form-urlencoded'
        );
        request.send(parameters.join('&'));
    }

    var buttons = form.getElementsByTagName('button');
    for (var j in buttons) {
        var button = buttons[j];
        if (button instanceof HTMLButtonElement) {
            // We cannot disable the button right now, as then it's value
            // is not sent to the server
            // See https://sarbbottam.github.io/blog/2015/08/21/multiple-submit-buttons-and-javascript
            unsafeSetTimeout(
                function () {
                    this.setAttribute('disabled', 'disabled');
                }.bind(button),
                0
            );
        }
    }

    var alerts = form.getElementsByClassName('alert');
    for (var j in alerts) {
        var alert = alerts[j];
        if (alert instanceof HTMLDivElement) {
            alert.classList.add('dismiss');
        }
    }

    if (window['pauseAutoRefresh']) {
        window.pauseAutoRefresh();
    }
};

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

var datePickers = [];
function initDatePicker() {
    if (!('flatpickr' in window)) {
        return;
    }
    flatpickr("input[type='date']", {
        altFormat: 'd.m.y',
    });
    flatpickr("input[type='datetime-local']", {
        enableTime: true,
        time_24hr: true,
        dateFormat: 'Z',
        altInput: true,
        altFormat: 'd.m.y, H:i',
    });
}

function removeDatePickers() {
    for (var i = 0; i < datePickers.length; i++) {
        console.log(datePickers[i]);
        datePickers[i].destroy();
    }
    datePickers = [];
}

var locked = false;
window.transitionToNewPage = function (newHtml) {
    if (locked) {
        console.warn(
            'transitionToNewPage: Did not execute transition due to lock'
        );
        return;
    }

    document.dispatchEvent(ihpUnloadEvent);

    var isModalOpen = document.body.classList.contains('modal-open');
    morphdom(
        document.body,
        newHtml.tagName === 'BODY' ? newHtml : newHtml.body,
        {
            childrenOnly: false,
            onBeforeElUpdated: function (from, to) {
                if (
                    newHtml.body &&
                    newHtml.body.classList.contains('modal-open') &&
                    from.id === 'main-row'
                ) {
                    return false;
                } else if (isModalOpen && from.id === 'main-row') {
                    return false;
                } else if (
                    from.classList.contains('flatpickr-input') &&
                    from._flatpickr
                ) {
                    unsafeSetTimeout(
                        function (from, to) {
                            console.log(
                                'FROM',
                                from,
                                to.getAttribute('value'),
                                to.value
                            );
                            from.value = to.value;
                            // from.setAttribute('value', to.getAttribute('value'));
                        },
                        0,
                        from,
                        to
                    );
                }
            },
            getNodeKey: function (el) {
                var key = el.id;
                if (el.id) {
                    key = el.id;
                } else if (el instanceof HTMLScriptElement) {
                    key = el.src;
                }
                return key;
            },
        }
    );

    var ihpAutoRefreshId =
        newHtml.head &&
        newHtml.head.querySelector('meta[property="ihp-auto-refresh-id"]');

    if (ihpAutoRefreshId) {
        var prevIhpAutoRefreshId = document.head.querySelector(
            'meta[property="ihp-auto-refresh-id"]'
        );
        if (prevIhpAutoRefreshId) {
            prevIhpAutoRefreshId.remove();
        }

        document.head.appendChild(ihpAutoRefreshId);
    }

    window.clearAllIntervals();
    window.clearAllTimeouts();

    locked = true;

    setTimeout(function () {
        locked = false;
    }, 1);
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
