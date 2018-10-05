open Containers
open Dynamic_grid
open Dashboard_common

type settings =
  { widget : Widget.t
  ; ready : bool React.signal
  ; set : unit -> (unit, string) Lwt_result.t
  }

type info =
  { title : string
  ; description : string
  ; thumbnail : [`Icon of string]
  ; serialized : Yojson.Safe.json
  } [@@deriving yojson]

type 'a item =
  { name : string
  ; settings : settings option
  ; widget : (#Widget.t as 'a)
  }

type 'a positioned_item =
  { item : 'a
  ; position : Position.t
  } [@@deriving yojson]

let make_info ?(description = "") ?(thumbnail = `Icon "help")
            ~(serialized : Yojson.Safe.json) ~(title : string) () =
  { title; thumbnail; description; serialized }

let make_item ?settings ~(name : string) (widget : #Widget.t) =
  { name; settings; widget }

let connect_apply (b : #Button.t) (settings : settings) =
  let s = React.S.map (fun x -> b#set_disabled @@ not x) settings.ready in
  b#listen_lwt Widget.Event.click (fun _ _ ->
      (match React.S.value settings.ready with
       | true -> settings.set ()
       | false -> Lwt_result.fail "no settings available")
      |> Lwt.map ignore) |> Lwt.ignore_result;
  s

class t ~(item : 'a item) () =
  let title = new Card.Primary.title item.name () in
  let close = Icon.SVG.(create_simple Path.close) in
  let remove = new Icon_button.t ~icon:close () in
  let sd =
    Option.map (fun (s : settings) ->
        let icon = Icon.SVG.(create_simple Path.settings) in
        let settings = new Icon_button.t ~icon () in
        let cancel = new Dialog.Action.t ~typ:`Cancel ~label:"Отмена" () in
        let apply = new Dialog.Action.t ~typ:`Accept  ~label:"ОК" () in
        let dialog =
          new Dialog.t
            ~title:(Printf.sprintf "Настройки. %s" item.name)
            ~content:(`Widgets [s.widget])
            ~actions:[ cancel; apply ]
            ()
        in
        let _ = connect_apply apply s in
        settings, dialog)
      item.settings in
  let icons = match sd with
    | Some (s, _) -> [s#widget]
    | None -> [] in
  let buttons = new Card.Actions.Icons.t ~widgets:icons () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  let heading = new Card.Primary.t ~widgets:[title#widget; actions#widget] () in
  let content = new Card.Media.t ~widgets:[item.widget] () in
  object(self)

    val mutable _editable = false
    inherit Card.t ~widgets:[ heading#widget
                            ; (new Divider.t ())#widget
                            ; content#widget ] () as super
    method remove  = remove
    method content = content
    method heading = heading

    method show_settings_button (x : bool) : unit = ()

    method editable = _editable
    method set_editable (x : bool) : unit =
      _editable <- x;
      if x then buttons#append_child remove
      else buttons#remove_child remove;
      item.widget#add_or_remove_class x Markup.Item.editing_class;
      List.iter (fun x -> x#layout ()) icons

    initializer
      Option.iter (fun ((settings : #Widget.t), (dialog : Dialog.t)) ->
          let open Lwt.Infix in
          settings#listen_click_lwt (fun _ _ ->
              dialog#show_await () >|= ignore) |> Lwt.ignore_result;
          Dom.appendChild Dom_html.document##.body dialog#root) sd;
      self#add_class Markup.Item._class;
      content#add_class Markup.Item.content_class;
      heading#add_class Markup.Item.heading_class;
      buttons#add_class Markup.Item.buttons_class
  end

let make item =
  new t ~item ()
