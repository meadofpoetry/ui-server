var open     = 'mdc-drawer--open';
var hide     = 'hide-sidedrawer';
var drawerEl = document.getElementById('mdc-drawer');
var btn      = document.getElementById('demo-menu');
var body     = document.body;
var hidden         = 'mdc-drawer__wrapper--hidden';
var not_hid        = 'mdc-drawer__wrapper';
var panel_open     = 'mdc-drawer__panel--open';
var activatedClass = 'mdc-list-item--selected';
var item_open      = 'mdc-tree__item--open';
var wrapper        = document.getElementById('drawer-wrapper');
var panel          = document.getElementById('drawer-panel');
var elementList    = document.querySelectorAll('.mdc-tree__item');
function toggle () {
    if (drawerEl.classList.contains(open))
    { localStorage.setItem('toolbar','');}
    else { localStorage.setItem('toolbar','open');};
    drawerEl.classList.toggle(open);
    body.classList.toggle(hide);
}
function resize () {
    window.setTimeout(function () {window.dispatchEvent(new Event('resize'));}, 225);
}
btn.addEventListener('click', function () {
    toggle ();
    resize ();
});
drawerEl.addEventListener('click', function () {
    toggle ();
    resize();
});
drawerEl.querySelector('.mdc-drawer__drawer').addEventListener('click', function(event) {
    var el = event.target;
    while (el && !el.classList.contains('mdc-list-item')) {
        el = el.parentElement;
    }
    if (el) {
        var activatedItem = document.querySelector('.' + activatedClass);
        if (activatedItem) {
            activatedItem.classList.remove(activatedClass);
        }
        event.target.classList.add(activatedClass);
    }
});
panel.addEventListener('click', function () {
    wrapper.classList.toggle(hidden);
    panel.classList.toggle(panel_open);
});
for(var i = 0; i <= elementList.length - 1; i++)
{ var item = elementList.item(i);
  if (item.querySelector('.mdc-tree__list'))
  { item.onclick = function () {this.classList.toggle(item_open);};
  };
};
