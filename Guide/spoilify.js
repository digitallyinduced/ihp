function spoilify () {
    var mac = document.getElementById("mac");
    var linux = document.getElementById("linux");
    var windows = document.getElementById("windows");

    var macSection = [mac];
    var linuxSection = [linux];
    var windowsSection = [windows];
    var entryPoint = mac.previousElementSibling;

    var section = mac.parentElement.children;
    var currentSection = "mac"
    function fetch() {
        for (var i = 0; i < section.length; i++) {
            if (section[i].id == "mac") {
                var macId = i;
                i = section.length;
            }
        }
        for (var i = macId; i < section.length; i++) {
            switch (section[i].id) {
                case "mac":
                    currentSection = "mac";
                    break;
                case "linux":
                    currentSection = "linux";
                    break;
                case "windows":
                    currentSection = "windows";
                    break;
                case "windows-firewall":
                    break;
                case "":
                    break;
                default:
                    if (section[i].nodeName = "H2") {
                        return;
                    }
            }
            switch (section[i].nodeName) {
                case "H3":
                    if (currentSection != section[i].id)
                    break;
                default:
                    switch (currentSection) {
                        case "mac":
                            macSection.push(section[i]);
                            break;
                        case "linux":
                            linuxSection.push(section[i]);
                            break;
                        case "windows":
                            windowsSection.push(section[i]);
                            break;
                    }
            }
        }
    }
    fetch();
    var macSpoiler = createSpoiler(mac, macSection);
    var linuxSpoiler = createSpoiler(linux, linuxSection);
    var windowsSpoiler = createSpoiler(windows, windowsSection);

    entryPoint.parentNode.insertBefore(windowsSpoiler, entryPoint.nextSibling);
    entryPoint.parentNode.insertBefore(linuxSpoiler, entryPoint.nextSibling);
    entryPoint.parentNode.insertBefore(macSpoiler, entryPoint.nextSibling);
}

function createSpoiler (head, elements) {
    head.style.marginTop = 0;
    head.style.display = "inline";
    head.style.border = "black";
    var summary = document.createElement("summary");
    var spoiler = document.createElement("details");
    spoiler.appendChild(summary);
    for (var i = 0; i < elements.length; i++) {
        spoiler.appendChild(elements[i]);
    }
    summary.appendChild(head);
    spoiler.classList = "rounded p-2 w-100 mb-3"
    spoiler.style.border = "1px solid rgba(13, 99, 120, 0.8)"
    return spoiler;
}