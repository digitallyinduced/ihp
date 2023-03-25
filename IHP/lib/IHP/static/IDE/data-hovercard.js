$(document).on('ready turbolinks:load', function () {
    initDataHoverCard();
});

function initDataHoverCard() {
    $('td[data-foreign-key-column]').each(function () {
        var element = this;

        $(element).tooltip({
            title: "Loading",
            html: true,
            placement: 'left',
            template: '<div class="tooltip foreign-key-hovercard" role="tooltip"><div class="arrow"></div><div class="tooltip-inner"></div></div>',
            boundary: 'window'
        });

        var hoverCard = null;

        // When the `_id` column doesn't have a foreign key constraint
        // E.g. it's a stripe_customer_id column, then we get a 404 error
        // by the server. In that case we just dispose the tooltip in `updateConte`
        var isError = false;

        function updateContent() {
            const tooltipId = element.getAttribute('aria-describedby');
            const tooltipEl = document.getElementById(tooltipId);

            if (!tooltipEl) {
                return;
            }
            if (isError) {
                $(element).tooltip('dispose');
                return;
            }
            const tooltipInner = tooltipEl.querySelector('.tooltip-inner');

            tooltipInner.innerHTML = hoverCard;

            $(element).attr('title', hoverCard)
            $(element).tooltip('update');
            $(element).tooltip('_fixTitle');
        }

        $(element).on('show.bs.tooltip', async function () {
            var url = element.dataset.foreignKeyColumn;
            if (!hoverCard) {
                hoverCard = fetch(url, { credentials: 'include' }).then(res => {
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
        $(element).on('shown.bs.tooltip', async function () {
            if (!(hoverCard instanceof Promise) && !element.getAttribute('title')) {
                updateContent();
            }
        })
    })
}