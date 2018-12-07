open Containers
open Dynamic_grid
open Dashboard_common

class type settings_view =
  object
    inherit Widget.t
    method apply : unit -> unit
    method reset : unit -> unit
  end

type settings =
  { widget : settings_view
  ; ready : bool React.signal
  ; set : unit -> (unit, string) Lwt_result.t
  }

type info =
  { title : string
  ; description : string
  ; thumbnail : [`Icon of string]
  ; serialized : Yojson.Safe.json
  } [@@deriving yojson]

type timestamp =
  { time : Ptime.t option React.signal
  ; to_string : Ptime.t -> string
  }

type subtitle =
  | Timestamp of timestamp

type 'a item =
  { name : string
  ; subtitle : subtitle option
  ; settings : settings option
  ; widget : (#Widget.t as 'a)
  }

type 'a positioned_item =
  { item : 'a
  ; position : Position.t
  } [@@deriving yojson, eq]

let make_timestamp ~time ~to_string () =
  { time; to_string }

let make_settings ?(ready = React.S.const true)
      ~(widget : #settings_view) ~set () : settings =
  { widget = (widget :> settings_view); ready; set }

let make_info ?(description = "") ?(thumbnail = `Icon "help")
      ~(serialized : Yojson.Safe.json) ~(title : string) () : info =
  { title; thumbnail; description; serialized }

let make_item ?settings ?subtitle ~(name : string)
      (widget : #Widget.t) : 'a item =
  { name; subtitle; settings; widget }

let make_timestamp_string to_string (time : Ptime.t option) : string =
  let prefix = "Последнее обновление: " in
  let suffix = match time with
    | None -> "-"
    | Some t -> to_string t in
  prefix ^ suffix

(** Creates settings dialog *)
let make_dialog (item : 'a item) (settings : settings) : Dialog.t =
  let cancel_button = new Button.t ~label:"Отмена" () in
  let apply_button = new Button.t ~label:"ОК" () in
  let s = React.S.map Fun.(apply_button#set_disabled % not)
            settings.ready in
  let cancel = Dialog.Action.make ~typ:`Cancel cancel_button in
  let apply = Dialog.Action.make ~typ:`Accept apply_button in
  let dialog =
    new Dialog.t
      ~title:(Printf.sprintf "Настройки. %s" item.name)
      ~content:(`Widgets [settings.widget])
      ~actions:[cancel; apply]
      () in
  dialog#set_on_destroy (fun () ->
      React.S.stop ~strong:true s);
  dialog

(** Shows the settings dialog *)
let show_dialog (item : 'a item) (dialog : Dialog.t) : unit Lwt.t =
  let open Lwt.Infix in
  dialog#show_await ()
  >>= function
  | `Cancel ->
     Option.iter (fun (x : settings) ->
         x.widget#reset ())
       item.settings;
     Lwt.return_unit
  | `Accept ->
     begin match item.settings with
     | None -> Lwt.return_unit
     | Some (s : settings) ->
        if React.S.value s.ready
        then (s.widget#apply ();
              s.set () >|= function _ -> ())
        else Lwt.return_unit
     end

class t ?(removable = true)
        ~(item : 'a item)
        () =
  let title = new Card.Primary.title item.name () in
  let subtitle, _ = match item.subtitle with
    | None -> None, None
    | Some (Timestamp { to_string; time }) ->
       let w = new Card.Primary.subtitle "" () in
       let s =
         React.S.map (fun x ->
             let str = make_timestamp_string to_string x in
             w#set_text_content str) time in
       Some w, Some s in
  let title_box =
    new Vbox.t
      ~widgets:(title :: (List.cons_maybe subtitle []))
      () in
  let remove =
    let icon = Icon.SVG.(create_simple Path.close) in
    new Icon_button.t ~icon () in
  let settings_icon, dialog =
    Option.map (fun (settings : settings) ->
        let icon = Icon.SVG.(create_simple Path.settings) in
        let icon_button = new Icon_button.t ~icon () in
        let dialog = make_dialog item settings in
        icon_button, dialog) item.settings
    |> function Some (i, d) -> Some i, Some d | None -> None, None in
  let icons = match settings_icon with
    | Some icon -> [icon#widget]
    | None -> [] in
  let buttons = new Card.Actions.Icons.t ~widgets:icons () in
  let actions = new Card.Actions.t ~widgets:[buttons] () in
  let heading =
    new Card.Primary.t
      ~widgets:[title_box#widget; actions#widget]
      () in
  let content = new Card.Media.t ~widgets:[item.widget] () in
  object(self)

    val mutable _listener = None
    val mutable _editable = false
    val mutable _removable = removable

    inherit Card.t ~widgets:[ heading#widget
                            ; (new Divider.t ())#widget
                            ; content#widget ] () as super

    method! init () : unit =
      super#init ();
      begin match settings_icon, dialog with
      | None, _ | _, None -> ()
      | Some btn, Some dlg ->
         btn#listen_click_lwt (fun _ _ -> show_dialog item dlg)
         |> (fun l -> _listener <- Some l);
         Widget.append_to_body dlg
      end;
      self#add_class Markup.Item._class;
      title_box#add_class Markup.Item.title_class;
      content#add_class Markup.Item.content_class;
      item.widget#add_class Markup.Item.widget_class;
      heading#add_class Markup.Item.heading_class;
      buttons#add_class Markup.Item.buttons_class

    method! destroy () : unit =
      super#destroy ();
      item.widget#remove_class Markup.Item.widget_class;
      Option.iter Lwt.cancel _listener;
      _listener <- None;
      Option.iter Widget.destroy settings_icon;
      Option.iter Fun.(Widget.(destroy % tap remove_from_body)) dialog

    method remove = remove

    method set_removable (x : bool) =
      if not @@ Equal.bool _removable x
      then if x then buttons#append_child remove
           else buttons#remove_child remove;
      _removable <- x

    method set_editable (x : bool) : unit =
      _editable <- x;
      if x && _removable
      then buttons#append_child remove
      else buttons#remove_child remove;
      item.widget#add_or_remove_class x Markup.Item.editing_class

  end

let make item =
  new t ~item ()
