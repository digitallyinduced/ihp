// To create a new Context Menu:
// - Add type to menus array
// - Add menu elements to html (ToolServer/Layout.hs (contextMenus)) with class="menu-for-TYPE"
// - Add class="object-TYPE" to every object the menu should open on
// - Add ".menu-for-TYPE" to CSS
var baseUrl = window.location.protocol + "//" + window.location.hostname + ":" + window.location.port + "/turbohaskell/";

var mapEvents = function () {
    console.log("Mapping..");
    var menus = ["column", "table", "enum"];
    for (var m = 0; m < menus.length; m++) {
        var objects = document.getElementsByClassName("context-" + menus[m]);
        for (var i = 0; i < objects.length; i++) {
            switch (menus[m]) {
                case "column":
                    objects[i].addEventListener("contextmenu",function(event){openMenu ("column", event, this);}, false);
                break;
                case "table":
                    objects[i].addEventListener("contextmenu",function(event){openMenu ("table", event, this);}, false);
                break;
                case "enum":
                    objects[i].addEventListener("contextmenu",function(event){openMenu ("enum", event, this);}, false);
                break;
            }
            
        }
    }
};

$( document ).ready(function() {
    mapEvents();
});

function openMenu (name, event, contextObject) {
    event.preventDefault();
    $(".menu-for-" + name).finish().toggle(100).
    css({
        top: event.pageY + "px",
        left: event.pageX + "px"
    });
    var contextElement = contextObject.parentElement;
    var menuoptions = document.getElementById('menu-' + name).children;
    for (var mo = 0; mo < menuoptions.length; mo++) {
        switch (menuoptions[mo].getAttribute('data-action')) {
            case "edit-table":
                var editTableButton = contextElement.children[1].children[1];
                menuoptions[mo].addEventListener("click", function (){window.location = editTableButton.href;}, false);
                break;
            case "delete-table":
                var deleteTableButton = contextElement.children[1].children[2];
                var deleteTable = function(){ // js-delete
                    if (!confirm('Are you sure you want to delete this?')) {
                        return;
                    }
                    var form = document.createElement('form');
                    form.action = deleteTableButton.href;
                    form.method = 'POST';
                    var methodInput = document.createElement('input');
                    methodInput.type = 'hidden';
                    methodInput.name = '_method';
                    methodInput.value = 'DELETE';
                    form.appendChild(methodInput);
                    document.body.appendChild(form);
                    window.submitForm(form);
                    this.removeEventListener("click", deleteTable, false);
                    mapEvents();
                };
                menuoptions[mo].addEventListener("click", deleteTable, false);
                break;
            case "delete-enum":
                var deleteTableButton = contextElement.children[1].children[2];
                var deleteTable = function(){ // js-delete
                    if (!confirm('Are you sure you want to delete this?')) {
                        return;
                    }
                    var form = document.createElement('form');
                    form.action = deleteTableButton.href;
                    form.method = 'POST';
                    var methodInput = document.createElement('input');
                    methodInput.type = 'hidden';
                    methodInput.name = '_method';
                    methodInput.value = 'DELETE';
                    form.appendChild(methodInput);
                    document.body.appendChild(form);
                    window.submitForm(form);
                    this.removeEventListener("click", deleteTable, false);
                    mapEvents();
                };
                menuoptions[mo].addEventListener("click", deleteTable, false);
                break;
            case "add-table":
                menuoptions[mo].addEventListener("click", function (){doAction("NewTable")}, false);
                break;
            case "add-enum":
                menuoptions[mo].addEventListener("click", function (){doAction("NewEnum")}, false);
                break;
            case "add-to-table":
                var tableName = contextObject.textContent;
                menuoptions[mo].addEventListener("click", function (){doAction("NewColumn?tableName=" + tableName)}, false);
                break;
            case "edit-column":
                var editColumnButton = contextElement.children[4].children[0];
                menuoptions[mo].addEventListener("click", function (){window.location = editColumnButton.href;}, false);
                break;
            case "delete-column":
                var deleteColumnButton = contextElement.children[4].children[1];
                var deleteColumn = function(){ // js-delete
                    if (!confirm('Are you sure you want to delete this?')) {
                        return;
                    }
                    var form = document.createElement('form');
                    form.action = deleteColumnButton.href;
                    form.method = 'POST';
                    var methodInput = document.createElement('input');
                    methodInput.type = 'hidden';
                    methodInput.name = '_method';
                    methodInput.value = 'DELETE';
                    form.appendChild(methodInput);
                    document.body.appendChild(form);
                    window.submitForm(form);
                    this.removeEventListener("click", deleteColumn, false);
                    mapEvents();
                };
                menuoptions[mo].addEventListener("click", deleteColumn, false);
                break;
            case "make-unique":
                
                break;
            case "add-column":
                menuoptions[mo].addEventListener("click", function (){window.location = document.getElementById("new-column").getAttribute("href");}, false);
                break;
            case "edit-enum":
                var editEnumButton = contextElement.children[1].children[1];
                menuoptions[mo].addEventListener("click", function (){window.location = editEnumButton.href;}, false);
                break;
            case "add-to-enum":
                var tableName = contextObject.textContent;
                menuoptions[mo].addEventListener("click", function (){doAction("NewEnumValue?enumName=" + tableName)}, false);
                break;
        }
    }
}

function jsdelete () {
    if (!confirm('Are you sure you want to delete this?')) {
        return;
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
    window.submitForm(form);
}

function doAction (actionUrl) {
    window.location = baseUrl + actionUrl;
}

// If the document is clicked somewhere
$(document).bind("mousedown", function (e) {
    
    // If the clicked element is not the menu
    if (!$(e.target).parents(".custom-menu").length > 0) {
        // Hide it
        console.log("close " + $(e.target).parents(".custom-menu").length);
        $(".custom-menu").hide(100);
    }
});

$(document).bind("click", function (e) {
    if (e.target.getAttribute("href") != null) {
        console.log("click");
        window.location = e.target.getAttribute("href");
    }
});

// If the menu element is clicked
$(".custom-menu li").click(function(){
    
    // This is the triggered action name
    switch($(this).attr("data-action")) {
        
        // A case for each action. Your actions here
        case "first": alert("first"); break;
        case "second": alert("second"); break;
        case "third": alert("third"); break;
    }
  
    // Hide it AFTER the action was triggered
    $(".custom-menu").hide(100);
  });