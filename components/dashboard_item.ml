open Containers
open Dynamic_grid
open Dashboard_common

let item_class    = Markup.CSS.add_element base_class "item"
let content_class = Markup.CSS.add_element item_class "content"
let heading_class = Markup.CSS.add_element item_class "heading"
let buttons_class = Markup.CSS.add_element item_class "heading-buttons"
let button_class  = Markup.CSS.add_element item_class "heading-button"
let editing_class = Markup.CSS.add_modifier item_class "editing"

type settings =
  { widget : Widget.widget
  ; ready  : bool React.signal
  ; set    : unit -> (unit,string) Lwt_result.t
  }

type info =
  { title       : string
  ; description : string
  ; thumbnail   : [`Icon of string]
  ; serialized  : Yojson.Safe.json
  } [@@deriving yojson]

type item =
  { name        : string
  ; settings    : settings option
  ; widget      : Widget.widget
  }

let to_info ?(description="") ?(thumbnail=`Icon "help")
            ~(serialized:Yojson.Safe.json) ~(title:string) () =
  { title; thumbnail; description; serialized }

let to_item ?settings ~(name:string) (widget:#Widget.widget) =
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
  let heading  = new Card.Primary.t ~widgets:[title#widget] () in
  let content  = new Card.Media.t ~widgets:[item.widget] () in
  object(self)
    val mutable _editable = false
    inherit Widget.widget (Dom_html.createDiv Dom_html.document) () as super
    method remove  = remove
    method content = content
    method heading = heading

    method editable       = _editable
    method set_editable x =
      _editable <- x;
      if x then (if self#editable then Dom.appendChild self#heading#root buttons#root)
      else (try Dom.removeChild self#heading#root buttons#root with _ -> ());
      item.widget#add_or_remove_class x editing_class;
      List.iter (fun x -> x#layout ()) buttons#widgets

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
