var open     = 'mdc-drawer--open';
var hide     = 'hide-sidedrawer';
var drawerEl = document.getElementById('main-drawer');
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
var list_open = 'mdc-tree__list--open';
for(var i = 0; i <= elementList.length - 1; i++)
{ var item = elementList.item(i);
  item.onclick = function () {this.classList.toggle(item_open);
                              var list = this.querySelector('.mdc-tree__list');
                              list.classList.toggle(list_open);};
};

function tree (el,n) {
    var items = el.querySelectorAll('.mdc-list-item');
    for(var j=0; j <= items.length-1; j++)
    {
        var item = items.item(j);
        if (item.classList.contains('mdc-tree__item'))
        {
            tree (item, (n+2));
        }
        else
        {
            item.style.paddingLeft = ((n+1)*16)+"px";
        };
    }
};

var lst = drawerEl.querySelectorAll('.mdc-tree__list');
for(var k=0; k <= lst.length-1; k++)
{
    tree (lst.item(k), 1);
};
