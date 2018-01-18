var tree         = document.getElementById('nav-inputs');
var nested       = document.getElementById('nav-inputs-nested');
var icon         = tree.querySelector(".mdc-list-item__end-detail");

var hidden_class = 'mdc-tree__item__nested-list--hidden';

// start by hiding the element
nested.classList.add(hidden_class);

function toggle() {
    nested.classList.toggle(hidden_class);
    icon.classList.toggle('mdc-expand--rotated');
    // if (nested.classList.contains(hidden_class)) {
    //     icon.classList.remove = "mdc-expand--rotated";
    // } else {
    //     icon.classList.add = "mdc-expand--rotated";
    // }
}

tree.addEventListener('click', toggle);
