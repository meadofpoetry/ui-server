open Components

type action =
  { callback : unit -> unit
  ; name : string
  ; icon : string
  }

let icon_button_of_action action =
  let icon =
    Icon_button.make
      ~on_click:(fun _ _ -> action.callback (); Lwt.return_unit)
      ~icon:(Icon.SVG.make_simple action.icon)#root
      () in
  icon#set_attribute "title" action.name;
  icon

let menu_item_of_action action =
  Item_list.Item.make
    ~role:"menuitem"
    ~graphic:Icon.SVG.(make_simple action.icon) (* FIXME *)
    action.name
