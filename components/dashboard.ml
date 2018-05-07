open Containers
open Dynamic_grid

let base_class    = "mdc-dashboard"
let item_class    = Markup.CSS.add_element base_class "item"
let content_class = Markup.CSS.add_element item_class "content"
let heading_class = Markup.CSS.add_element item_class "heading"
let buttons_class = Markup.CSS.add_element item_class "heading-buttons"
let button_class  = Markup.CSS.add_element item_class "heading-button"

module Position = Dynamic_grid.Position

module Item = struct

  type 'a settings =
    { widget    : Widget.widget
    ; signal    : 'a option React.signal
    ; set       : 'a -> (unit,string) Lwt_result.t
    }

  type 'a item =
    { name        : string
    ; settings    : 'a settings option
    ; widget      : Widget.widget
    }

  let to_item ?settings ~name (widget:#Widget.widget) =
    { name; settings; widget = widget#widget }

  let connect_apply (b:#Button.t) (settings:'a settings) =
    let s = React.S.map (function Some _ -> b#set_disabled false | None -> b#set_disabled true) settings.signal in
    let e = React.E.map (fun _ -> match React.S.value settings.signal with
                                  | Some x -> settings.set x
                                  | None   -> Lwt_result.fail "no settings available") b#e_click
    in s,e

  class t ~(item:'a item) () =
    let title    = new Card.Primary.title item.name () in
    let remove   = new Icon.Button.Font.t ~icon:"close" () in
    let sd       =
      Option.map (fun (s:'a settings) ->
          let settings = new Icon.Button.Font.t ~icon:"settings" () in
          let cancel   = new Dialog.Action.t ~typ:`Decline ~label:"Отмена" () in
          let apply    = new Dialog.Action.t ~typ:`Accept  ~label:"ОК" () in
          let dialog   = new Dialog.t
                             ~title:(Printf.sprintf "Настройки. %s" item.name)
                             ~content:(`Widgets [s.widget])
                             ~actions:[ cancel; apply ]
                             ()
          in
          let _ = connect_apply apply s in
          settings,dialog)
                 item.settings
    in
    let buttons  = let widgets = match sd with
                     | Some (s,_) -> [s#widget;remove#widget]
                     | None       -> [remove#widget]
                   in
                   List.iter (fun x -> x#add_class button_class) widgets;
                   new Box.t ~vertical:false ~widgets ()
    in
    let heading  = new Card.Primary.t ~widgets:[title#widget;buttons#widget] () in
    let content  = new Card.Media.t ~widgets:[item.widget] () in
    object(self)
      inherit Widget.widget (Dom_html.createDiv Dom_html.document) () as super
      method remove  = remove
      method content = content
      method heading = heading
      initializer
        Option.iter (fun (s,d) -> let open Lwt.Infix in
                                  React.E.map (fun _ -> d#show) s#e_click |> ignore;
                                  Dom.appendChild Dom_html.document##.body d#root) sd;
        self#add_class item_class;
        content#add_class content_class;
        heading#add_class heading_class;
        buttons#add_class buttons_class;
        Dom.appendChild self#root heading#root;
        Dom.appendChild self#root content#root
    end

  let make item = new t ~item ()

  type positioned_item =
    { item     : t
    ; position : Position.t
    }

end

module type Factory = sig
  type 'a item
  type init

  class type t =
    object
      method create  : 'a. 'a item -> 'a Item.item
      method destroy : unit -> unit
    end

  val make : init -> t
end

class t ~(items:Item.positioned_item list) () =
  let get   = fun (i:Item.positioned_item) ->
    Dynamic_grid.Item.to_item ~close_widget:i.item#remove#widget
                              ~widget:i.item#widget
                              ~value:()
                              ~pos:i.position
                              ()
  in
  let grid  = to_grid ~vertical_compact:true ~row_height:150 ~items_margin:(10,10) ~cols:4 () in
  object(self)
    inherit [unit,
             unit Dynamic_grid.Item.t,
             Item.positioned_item] Dynamic_grid_abstract.t ~items:[] ~get ~grid () as super

    method serialize () : Yojson.Safe.json = Yojson.Safe.from_string "\"\""

    initializer
      self#set_on_load @@ Some (fun () -> self#layout);
      self#add_class base_class;
      List.map self#add items |> ignore
  end
