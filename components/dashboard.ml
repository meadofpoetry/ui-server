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

  type settings =
    { widget : Widget.widget
    ; ready  : bool React.signal
    ; set    : unit -> (unit,string) Lwt_result.t
    }

  type item =
    { name        : string
    ; settings    : settings option
    ; widget      : Widget.widget
    }

  let to_item ?settings ~name (widget:#Widget.widget) =
    { name; settings; widget = widget#widget }

  let connect_apply (b:#Button.t) (settings:settings) =
    let s = React.S.map (fun x -> b#set_disabled @@ not x) settings.ready in
    let e = React.E.map (fun _ -> match React.S.value settings.ready with
                                  | true  -> settings.set ()
                                  | false -> Lwt_result.fail "no settings available") b#e_click
    in s,e

  class t ~(item:item) () =
    let title    = new Card.Primary.title item.name () in
    let remove   = new Icon.Button.Font.t ~icon:"close" () in
    let sd       =
      Option.map (fun (s:settings) ->
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
                                  React.E.map (fun _ -> d#show ()) s#e_click |> ignore;
                                  Dom.appendChild Dom_html.document##.body d#root) sd;
        self#add_class item_class;
        content#add_class content_class;
        heading#add_class heading_class;
        buttons#add_class buttons_class;
        Dom.appendChild self#root heading#root;
        Dom.appendChild self#root content#root
    end

  let make item = new t ~item ()

  type 'a positioned_item =
    { item     : 'a
    ; position : Position.t
    } [@@deriving yojson]

end

type 'a lst = 'a list [@@deriving yojson]

class type ['a] factory =
  object
    method create      : 'a -> Item.item
    method destroy     : unit -> unit
    method serialize   : 'a -> Yojson.Safe.json
    method deserialize : Yojson.Safe.json -> ('a,string) result
  end

class ['a] t ~(items:'a Item.positioned_item list) (factory:'a #factory) () =
  let get = fun (i:'a Item.positioned_item) ->
    factory#create i.item
    |> fun x -> Item.make x
                |> fun x -> Dynamic_grid.Item.to_item ~close_widget:x#remove#widget
                                                      ~widget:x#widget
                                                      ~value:i.item
                                                      ~pos:i.position
                                                      ()
  in
  let grid  = to_grid ~vertical_compact:true ~row_height:150 ~items_margin:(10,10) ~cols:4 () in
  object(self)
    inherit ['a,
             'a Dynamic_grid.Item.t,
             'a Item.positioned_item] Dynamic_grid_abstract.t ~items:[] ~get ~grid () as super

    method serialize () : Yojson.Safe.json =
      List.map (fun x -> let (i:'a Item.positioned_item) = { position = x#pos; item = x#value} in
                         Item.positioned_item_to_yojson factory#serialize i) self#items
      |> fun l -> `List l

    method deserialize (json:Yojson.Safe.json) : ('a Item.positioned_item list,string) result =
      lst_of_yojson (fun x -> Item.positioned_item_of_yojson factory#deserialize x) json
    method restore (json:Yojson.Safe.json) : (unit,string) result =
      self#deserialize json
      |> Result.map (fun l -> List.iter (fun x -> x#remove ()) self#items; (* remove previous items *)
                              List.iter (fun x -> self#add x |> ignore) l)

    method destroy () = factory#destroy ()

    initializer
      self#set_on_load @@ Some (fun () -> self#layout ());
      self#add_class base_class;
      List.map self#add items |> ignore
  end
