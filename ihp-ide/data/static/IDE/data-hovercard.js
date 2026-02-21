$(document).on('ready turbolinks:load', function () {
    initDataHoverCard();
});

// Dispose hovercards before Turbolinks replaces the page to prevent orphaned tooltip elements
document.addEventListener('turbolinks:before-render', function () {
    document.querySelectorAll('td[data-foreign-key-column]').forEach(function (el) {
        var existing = bootstrap.Tooltip.getInstance(el);
        if (existing) existing.dispose();
    });
});

function initDataHoverCard() {
    document.querySelectorAll('td[data-foreign-key-column]').forEach(function (element) {
        var bsTooltip = new bootstrap.Tooltip(element, {
            title: "Loading",
            html: true,
            placement: 'left',
            container: 'body',
            popperConfig: { strategy: 'fixed' },
            template: '<div class="tooltip foreign-key-hovercard" role="tooltip"><div class="tooltip-arrow"></div><div class="tooltip-inner"></div></div>'
        });

        var hoverCard = null;

        // When the `_id` column doesn't have a foreign key constraint
        // E.g. it's a stripe_customer_id column, then we get a 404 error
        // by the server. In that case we just dispose the tooltip
        var isError = false;

        function updateContent() {
            var tooltipEl = bsTooltip.tip;

            if (!tooltipEl) {
                return;
            }
            if (isError) {
                bsTooltip.dispose();
                return;
            }
            var tooltipInner = tooltipEl.querySelector('.tooltip-inner');

            tooltipInner.innerHTML = hoverCard;
            bsTooltip.update();
        }

        element.addEventListener('show.bs.tooltip', async function () {
            var url = element.dataset.foreignKeyColumn;
            if (!hoverCard) {
                hoverCard = fetch(url, { credentials: 'include' }).then(function (res) {
                    if (!res.ok) {
                        console.log('ERROR');
                        isError = true;
                        return;
                    }
                    return res.text();
                });
                hoverCard = await hoverCard;

                updateContent();
            }
        });
        element.addEventListener('shown.bs.tooltip', async function () {
            if (typeof hoverCard === 'string') {
                updateContent();
            }
        });
    });
}
