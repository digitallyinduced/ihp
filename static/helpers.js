document.addEventListener('DOMContentLoaded', function () {
    initDelete();
    initDisableButtonsOnSubmit();
    initBack();
    initToggle();
    initTime();
    initDatePicker();
});

document.addEventListener('turbolinks:load', function () {
    initBack();
    initToggle();
    initTime();

    setTimeout(function () {
        var elements = document.querySelectorAll('.js-scroll-into-view');
        for (var i in elements) {
            var element = elements[i];
            if (element && element.scrollIntoView)
                element.scrollIntoView({ behavior: 'smooth', block: 'start' });
        }
    }, 1);

    initDatePicker();
});

function initTime() {
    if (window.timeago)
        window.timeago().render(document.querySelectorAll('.time-ago'));

    var dateElements = document.querySelectorAll(".date-time");
    dateElements.forEach(function(elem){
        var date = new Date(elem.dateTime);
        elem.innerHTML = date.toLocaleDateString() +", " + date.toLocaleTimeString().substr(0,5)+" Uhr";
    });
}

function initDelete() {
    document.addEventListener('click', handleClick);

    function handleClick(event) {
        if (event.target instanceof HTMLAnchorElement && event.target.classList.contains('js-delete')) {
            event.preventDefault();

            if (!event.target.classList.contains('js-delete-no-confirm')) {
                if (!confirm('Are you sure you want to delete this?')) {
                    return;
                }
            }

            var form = document.createElement('form');
            form.action = event.target.href;
            form.method = 'POST';

            var methodInput = document.createElement('input');
            methodInput.type = 'hidden';
            methodInput.name = '_method';
            methodInput.value = 'DELETE';

            form.appendChild(methodInput);

            document.body.appendChild(form);
            submitForm(form);
        }
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
            console.error('js-back does not supports <a> elements, use a <button> instead', element);
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
        submitForm(form, lastClicked);
    });

    document.addEventListener('mouseup', function (event) {
        lastClicked = event.target;
    });
}

function submitForm(form, possibleClickedButton) {
    var request = new XMLHttpRequest();
    request.responseType = "document";
    request.overrideMimeType('text/html');
    request.onload = function () {
        console.info('AJAX', this.status, this.responseURL);
        if (request.readyState !== request.DONE) {
            return;
        }
        if (request.status !== 200) {
            console.error('Something went wrong, status code: ' + request.status);
            return;
        }

        if (window.Turbolinks) {
            var snapshot = new Turbolinks.Snapshot(new Turbolinks.HeadDetails(request.response.head), request.response.body);
            transitionToNewPage(request.response.body);
            Turbolinks.clearCache();
            history.pushState({}, '', request.responseURL);
            var turbolinkLoadEvent = new CustomEvent("turbolinks:load");
            document.dispatchEvent(turbolinkLoadEvent);
        } else {
            window.liveReloadPaused = true;
            console.log('Live Reload Paused');
            history.pushState({}, '', request.responseURL);

            transitionToNewPage(request.response.body);
            var turbolinkLoadEvent = new CustomEvent("turbolinks:load");
            document.dispatchEvent(turbolinkLoadEvent);

            var reenableLiveReload = function () {
                window.liveReloadPaused = false;
                console.log('Live Reload Re-Activated');

                document.removeEventListener('turbolinks:load', reenableLiveReload);
            };

            document.addEventListener('turbolinks:load', reenableLiveReload);

        }
    };
    request.open(form.method, form.action, true);

    var submit = document.activeElement;
    if (!submit || submit instanceof HTMLBodyElement) {
        submit = possibleClickedButton;
    }

    var formData = new FormData(form);
    console.log(form, formData, submit);

    if ((submit instanceof HTMLInputElement || (submit instanceof HTMLButtonElement && submit.getAttribute('type') == 'submit')) && submit.form == form) {
        formData.set(submit.getAttribute('name'), submit.value);
    }

    var parameters = []
    for (var pair of formData.entries()) {
        parameters.push(
            encodeURIComponent(pair[0]) + '=' +
            encodeURIComponent(pair[1])
        );
    }


    request.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
    request.send(parameters.join('&'));

    var buttons = form.getElementsByTagName('button');
    for (var j in buttons) {
        var button = buttons[j];
        if (button instanceof HTMLButtonElement) {
            // We cannot disable the button right now, as then it's value
            // is not sent to the server
            // See https://sarbbottam.github.io/blog/2015/08/21/multiple-submit-buttons-and-javascript
            setTimeout(function () { this.setAttribute('disabled', 'disabled'); }.bind(button), 0);
        }
    }

    var alerts = form.getElementsByClassName('alert');
    for (var j in alerts) {
        var alert = alerts[j];
        if (alert instanceof HTMLDivElement) {
            alert.classList.add('dismiss');
        }
    }
}

function initToggle() {
    var elements = document.querySelectorAll('[data-toggle]');

    function handler () {
        var elements = document.querySelectorAll(this.getAttribute('data-toggle'));
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

    };

    for (var i in elements) {
        var element = elements[i];
        if (!(element instanceof HTMLInputElement)) {
            continue;
        }

        element.addEventListener('change', handler);
        handler.call(element);
    }
}

function initDatePicker() {
    flatpickr("input[type='date']", {});
}

var locked = false;
var debugMorphdom = false;
window.transitionToNewPage = function (newBody) {
    if (locked) {
        return;
    }

    console.log(1, document.body)
    var isModalOpen = document.body.classList.contains('modal-open');
    morphdom(document.body, newBody, {
        childrenOnly: true,
        onBeforeElUpdated: function (from, to) {

            if (newBody.classList.contains('modal-open') && from.id === 'main-row') {
                return false;
            } else if (isModalOpen && from.id === 'main-row') {
                return false;
            } else if (from.parentNode && from.parentNode.tagName === 'svg') {
                return false;
            }

            if (debugMorphdom)
                console.log('onBeforeElUpdated', from, to)
        },
        onBeforeElChildrenUpdated: function (from, to) {
            if (debugMorphdom)
                console.log('onBeforeElChildrenUpdated', from, to);
        },
        onBeforeNodeDiscarded: function (el) {
            if (el && el.classList && el.classList.contains && el.classList.contains('animate-delete')) {
                el.style.height = window.getComputedStyle(el).height;
                // el.style.height = window.getComputedStyle(el).height;

                setTimeout(function (el) {
                    el.classList.add('delete');
                }, 0, el);
                setTimeout(function (el) {
                    el.parentNode.removeChild(el);
                }, 300, el);
                return false;
            }

            if (debugMorphdom)
                console.log('onBeforeNodeDiscarded', el);
        },
        getNodeKey: function (el) {
            var key = el.id;
            if (el.id) {
                key = el.id;
            } else if (el.form && el.name) {
                key = el.name + "_" + el.form.action;
            } else if (el instanceof HTMLFormElement) {
                key = "form#" + el.action;
            } else if (el instanceof HTMLScriptElement) {
                key = el.src;
            }
            return key;
        },
        onBeforeNodeAdded: function (el) {
            if (debugMorphdom)
                console.log('onBeforeNodeAdded', el);
        }
    });

    locked = true;

    setTimeout(function () {
        locked = false;
    }, 100);
}
