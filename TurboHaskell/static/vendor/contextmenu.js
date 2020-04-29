// To create a new Context Menu:
// - Add type to menus array
// - Add menu elements to html (ToolServer/Layout.hs (contextMenus)) with class="menu-for-TYPE"
// - Add class="object-TYPE" to every object the menu should open on
// - Add ".menu-for-TYPE" to CSS
var baseUrl = window.location.protocol + "//" + window.location.hostname + ":" + window.location.port + "/turbohaskell/";

$( document ).ready(function() {
    
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
                menuoptions[mo].addEventListener("click", function (){alert("add-to-table");}, false);
                break;
            case "edit-column":
                menuoptions[mo].addEventListener("click", function (){alert("edit-column");}, false);
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
        $(".custom-menu").hide(100);
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