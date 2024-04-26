function spoilify () {
    if (document.getElementById("mac") == null) {return;}

    // Headers for each section.
    const mac = document.getElementById("mac");
    const linux = document.getElementById("linux");
    const windows = document.getElementById("windows");

    var macSection = [mac];
    var linuxSection = [linux];
    var windowsSection = [windows];
    const entryPoint = mac.previousElementSibling;

    const section = mac.parentElement.children;
    var currentSection = "mac"

    // Build arrays of section contents to be collapsed into `<details>`.
    function fetch() {
        // First seek to the H3 with `id="mac"` and get its offset.
        for (var i = 0; i < section.length; i++) {
            if (section[i].id == "mac") {
                var macId = i;
                i = section.length;
            }
        }

        // Use the offset found above to loop through every element in `section` until:
        // every element is added to a section, or; (more likely) the loop hits a heading
        // element like h1, h2, or h3.
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
                default:
                    // There are H6 level headings inside sections, skip them without
                    // further processing.
                    // @todo: it seems this is superfluous.
                    if (section[i].nodeName == "H6") {
                        break;
                    // If the loop has hit another high-level header element, the section
                    // is complete.
                    } else if (["H1", "H2", "H3"].includes(section[i].nodeName)) {
                        return;
                    }
            }
            switch (section[i].nodeName) {
                // Each section's header is in an H3, and is handled outside this statement.
                case "H3":
                    if (currentSection != section[i].id)
                    break;
                default:
                    // Push the current DOM element onto the array of elements for the
                    // current section.
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
