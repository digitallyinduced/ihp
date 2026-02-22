(function () {
    var ihpLoadEvent = new Event('ihp:load');
    var ihpUnloadEvent = new Event('ihp:unload');

    function toArray(nodeList) {
        return Array.prototype.slice.call(nodeList || []);
    }

    function selectAll(root, selector) {
        var scope = root || document;
        var result = [];

        if (scope.matches && scope.matches(selector)) {
            result.push(scope);
        }

        if (scope.querySelectorAll) {
            result = result.concat(toArray(scope.querySelectorAll(selector)));
        }

        return result;
    }

    function dispatchIhpLoad() {
        document.dispatchEvent(ihpLoadEvent);
    }

    function dispatchIhpUnload() {
        document.dispatchEvent(ihpUnloadEvent);
    }

    function applyToggleInput(input) {
        var selector = input.getAttribute('data-toggle');
        if (!selector) {
            return;
        }

        toArray(document.querySelectorAll(selector)).forEach(function (el) {
            if (!(el instanceof HTMLElement)) {
                return;
            }

            if (input.checked) {
                el.removeAttribute('disabled');
            } else {
                el.setAttribute('disabled', 'disabled');
            }
        });
    }

    function handleToggleChange(event) {
        applyToggleInput(event.currentTarget);
    }

    function initToggle(root) {
        selectAll(root, '[data-toggle]').forEach(function (input) {
            if (!(input instanceof HTMLInputElement)) {
                return;
            }

            if (!input.__ihpToggleInitialized) {
                input.addEventListener('change', handleToggleChange);
                input.__ihpToggleInitialized = true;
            }

            applyToggleInput(input);
        });
    }

    function initTime(root) {
        if (window.timeago) {
            window.timeago().render(selectAll(root, '.time-ago'));
        }

        selectAll(root, '.date-time').forEach(function (elem) {
            var date = new Date(elem.dateTime);
            elem.innerHTML =
                date.toLocaleDateString() +
                ', ' +
                date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
        });

        selectAll(root, '.date').forEach(function (elem) {
            var date = new Date(elem.dateTime);
            elem.innerHTML = date.toLocaleDateString();
        });

        selectAll(root, '.time').forEach(function (elem) {
            var date = new Date(elem.dateTime);
            elem.innerHTML = date.toLocaleTimeString([], {
                hour: '2-digit',
                minute: '2-digit',
            });
        });
    }

    function handleFilePreviewChange(event) {
        var input = event.currentTarget;
        var previewSelector = input.getAttribute('data-preview');
        if (!previewSelector || !input.files || !input.files[0]) {
            return;
        }

        var target = document.querySelector(previewSelector);
        if (!target) {
            return;
        }

        var reader = new FileReader();
        reader.onload = function (e) {
            target.setAttribute('src', e.target.result);
        };
        reader.readAsDataURL(input.files[0]);
    }

    function initFileUploadPreview(root) {
        selectAll(root, 'input[type="file"]').forEach(function (input) {
            if (!(input instanceof HTMLInputElement)) {
                return;
            }

            if (!input.getAttribute('data-preview')) {
                return;
            }

            if (!input.__ihpFileUploadPreviewInitialized) {
                input.addEventListener('change', handleFilePreviewChange);
                input.__ihpFileUploadPreviewInitialized = true;
            }
        });
    }

    function initDatePicker(root) {
        if (!('flatpickr' in window)) {
            return;
        }

        selectAll(root, "input[type='date']").forEach(function (el) {
            if (el._flatpickr) {
                return;
            }

            var dateOptions = {};
            if (!el.dataset.altFormat) {
                dateOptions.altFormat = 'd.m.y';
            }
            if (!el.dataset.altInput) {
                dateOptions.altInput = true;
            }

            flatpickr(el, dateOptions);
        });

        selectAll(root, "input[type='datetime-local']").forEach(function (el) {
            if (el._flatpickr) {
                return;
            }

            var datetimeOptions = {};
            if (!el.dataset.enableTime) {
                datetimeOptions.enableTime = true;
            }
            if (!el.dataset.time_24hr) {
                datetimeOptions.time_24hr = true;
            }
            if (!el.dataset.dateFormat) {
                datetimeOptions.dateFormat = 'Z';
            }
            if (!el.dataset.altFormat) {
                datetimeOptions.altFormat = 'd.m.y, H:i';
            }
            if (!el.dataset.altInput) {
                datetimeOptions.altInput = true;
            }

            flatpickr(el, datetimeOptions);
        });
    }

    function initScrollIntoView(root) {
        var delay = window.unsafeSetTimeout || window.setTimeout;
        delay(function () {
            selectAll(root, '.js-scroll-into-view').forEach(function (el) {
                if (el && el.scrollIntoView) {
                    el.scrollIntoView({ behavior: 'smooth', block: 'start' });
                }
            });
        }, 1);
    }

    function handleBackClick(event) {
        event.preventDefault();
        var element = event.currentTarget;
        element.setAttribute('disabled', 'disabled');
        element.classList.add('disabled');
        window.history.back();
    }

    function initBack(root) {
        selectAll(root, '.js-back, [data-js-back]').forEach(function (element) {
            if (element.__ihpBackInitialized) {
                return;
            }

            if (element instanceof HTMLButtonElement || element.hasAttribute('data-js-back')) {
                element.addEventListener('click', handleBackClick);
            } else if (element instanceof HTMLAnchorElement && element.classList.contains('js-back')) {
                console.error(
                    'js-back does not supports <a> elements, use a <button> instead',
                    element
                );
            }

            element.__ihpBackInitialized = true;
        });
    }

    function dismissFormAlerts(form) {
        if (!form) {
            return;
        }

        toArray(form.querySelectorAll('.alert')).forEach(function (alert) {
            if (alert instanceof HTMLDivElement) {
                alert.classList.add('dismiss');
            }
        });
    }

    function initCompatibility(root, options) {
        var settings = options || {};

        initBack(root);
        initToggle(root);
        initTime(root);
        initDatePicker(root);
        initFileUploadPreview(root);

        if (settings.includeScrollIntoView) {
            initScrollIntoView(root);
        }
    }

    if (!('allIntervals' in window)) {
        window.allIntervals = [];
        window.allTimeouts = [];

        window.unsafeSetInterval = window.setInterval;
        window.unsafeSetTimeout = window.setTimeout;

        window.setInterval = function () {
            var id = window.unsafeSetInterval.apply(window, arguments);
            window.allIntervals.push(id);
            return id;
        };

        window.setTimeout = function () {
            var id = window.unsafeSetTimeout.apply(window, arguments);
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

    window.addEventListener('beforeunload', function () {
        dispatchIhpUnload();
    });

    document.addEventListener('DOMContentLoaded', function () {
        initCompatibility(document, { includeScrollIntoView: true });
        dispatchIhpLoad();
    });

    if (window.htmx) {
        document.addEventListener('htmx:beforeSwap', function () {
            dispatchIhpUnload();
        });

        document.addEventListener('htmx:load', function (event) {
            var root =
                event && event.detail && event.detail.elt
                    ? event.detail.elt
                    : event && event.target
                        ? event.target
                        : document;

            initCompatibility(root, { includeScrollIntoView: true });
            dispatchIhpLoad();
        });

        document.addEventListener('htmx:beforeRequest', function (event) {
            var trigger = event && event.detail ? event.detail.elt : null;
            var form = trigger && trigger.closest ? trigger.closest('form') : null;
            dismissFormAlerts(form);
        });
    }

    // HTMX morphdom swap extension.
    // Rationale:
    // - HTMX by default swaps via innerHTML; that nukes element identity and
    //   loses input state on full-page updates.
    // - morphdom preserves element identity by diffing the existing DOM, which
    //   keeps input values, cursor positions, and attached listeners stable.
    // - HTMX does not re-process nodes when the swap is handled by an extension,
    //   so we explicitly call htmx.process to activate new hx-* attributes.

    // Preserve user-entered values during swaps (especially for full-body refreshes).
    function syncInputValue(fromEl, toEl) {
        var tag = fromEl.tagName;
        if (tag === 'INPUT') {
            var type = (fromEl.getAttribute('type') || '').toLowerCase();
            if (type === 'checkbox' || type === 'radio') {
                toEl.checked = fromEl.checked;
            }
            if (type !== 'file') {
                toEl.value = fromEl.value;
            }
        } else if (tag === 'TEXTAREA') {
            toEl.value = fromEl.value;
        } else if (tag === 'OPTION') {
            toEl.selected = fromEl.selected;
        }
    }

    // Provide a stable key so morphdom can match nodes across updates.
    function getNodeKey(el) {
        if (el.id) {
            return el.id;
        }
        if (el instanceof HTMLScriptElement && el.src) {
            return el.src;
        }
        return undefined;
    }

    // HTMX gives us a fragment; wrap it to match the target element for morphdom.
    // morphdom expects a real element, not a bare document fragment.
    function toSwapNode(target, fragment) {
        if (fragment && fragment.nodeType === 11) {
            var wrapper = document.createElement(target.tagName);
            for (var i = 0; i < target.attributes.length; i++) {
                var attr = target.attributes[i];
                wrapper.setAttribute(attr.name, attr.value);
            }
            wrapper.appendChild(fragment);
            return wrapper;
        }
        return fragment;
    }

    // morphdom preserves element identity, which keeps inputs stable across swaps.
    // We also keep file inputs untouched because browsers restrict programmatic
    // value changes for security reasons.
    function morphdomSwap(target, fragment) {
        var swapNode = toSwapNode(target, fragment);
        morphdom(target, swapNode, {
            childrenOnly: false,
            getNodeKey: getNodeKey,
            onBeforeElUpdated: function (fromEl, toEl) {
                // File inputs are not safely transferable across DOM patches.
                if (fromEl.tagName === 'INPUT') {
                    var type = (fromEl.getAttribute('type') || '').toLowerCase();
                    if (type === 'file') {
                        return false;
                    }
                }
                syncInputValue(fromEl, toEl);
                return true;
            },
        });
    }

    if (window.htmx && window.morphdom) {
        htmx.defineExtension('morphdom-swap', {
            isInlineSwap: function (swapStyle) {
                return swapStyle === 'morphdom';
            },
            handleSwap: function (swapStyle, target, fragment, settleInfo) {
                if (swapStyle !== 'morphdom') return;
                // Swap using morphdom instead of the default innerHTML strategy.
                morphdomSwap(target, fragment);
                // morphdom swaps bypass HTMX's normal processing, so re-process manually.
                // This is required to rebind hx-get/hx-post/etc on freshly swapped nodes.
                if (settleInfo && settleInfo.tasks) {
                    settleInfo.tasks.push(function () {
                        htmx.process(target);
                    });
                } else {
                    htmx.process(target);
                }
                return true;
            },
        });
    }
})();
