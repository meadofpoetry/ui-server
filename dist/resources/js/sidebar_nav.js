var tree         = document.getElementById('nav-inputs');
var nested       = document.getElementById('nav-inputs-nested');
var icon         = tree.querySelector(".mdc-list-item__end-detail");

var hidden_class = 'mdc-tree__item__nested-list--hidden';

// start by hiding the element
nested.classList.add(hidden_class);
icon.innerText = "expand_more";

function toggle() {
    nested.classList.toggle(hidden_class);
    if (nested.classList.contains(hidden_class)) {
        icon.innerText = "expand_more";
    } else {
        icon.innerText = "expand_less";
    }
}

tree.addEventListener('click', toggle);
