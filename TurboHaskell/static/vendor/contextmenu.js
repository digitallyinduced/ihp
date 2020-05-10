var onClickHandler;
function showContextMenu(contextMenuId) {
    event.preventDefault();

    for (const element of document.querySelectorAll('.context-menu-open')) {
        element.classList.remove('context-menu-open');
    }

    if (onClickHandler) {
        document.removeEventListener('click', onClickHandler);
    }

    var contextMenu = document.getElementById(contextMenuId);
    contextMenu.style.top = String(event.pageY) + 'px';
    contextMenu.style.left = String(event.pageX) + 'px';

    contextMenu.classList.add('context-menu-open');

    onClickHandler = () => {
        contextMenu.classList.remove('context-menu-open');
        document.removeEventListener('click', onClickHandler);
    }

    document.addEventListener('click', onClickHandler);
}
