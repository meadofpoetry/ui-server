open Js_of_ocaml
open Components
open Dynamic_grid
open Dashboard_common

let ( % ) f g x = f (g x)

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
  (* let s = React.S.map Fun.(apply_button#set_disabled % not)
   *           settings.ready in *)
  let content = Dialog.Markup.create_content
      ~content:[Widget.to_markup settings.widget] () in
  let dialog =
    Dialog.make
      ~content
      ~title:(Dialog.Markup.create_title_simple
                (Printf.sprintf "Настройки. %s" item.name) ())
      ~actions:[ Dialog.Markup.create_action ~label:"Отмена" ~action:Close ()
               ; Dialog.Markup.create_action ~label:"OK" ~action:Accept () ]
      () in
  (* dialog#set_on_destroy (fun () ->
   *     React.S.stop ~strong:true s); *)
  dialog

(** Shows the settings dialog *)
let show_dialog (item : 'a item) (dialog : Dialog.t) : unit Lwt.t =
  let open Lwt.Infix in
  dialog#open_await ()
  >>= function
  | Accept ->
    begin match item.settings with
      | None -> Lwt.return_unit
      | Some (s : settings) ->
        if React.S.value s.ready
        then (s.widget#apply ();
              s.set () >|= function _ -> ())
        else Lwt.return_unit
    end
  | _ ->
    Utils.Option.iter (fun (x : settings) ->
        x.widget#reset ())
      item.settings;
    Lwt.return_unit

class t ?(removable = true)
        ~(item : 'a item)
        () =
  let title = Card.Primary.make_title item.name in
  let subtitle, _ = match item.subtitle with
    | None -> None, None
    | Some (Timestamp { to_string; time }) ->
      let w = Card.Primary.make_subtitle "" in
      let s =
        React.S.map (fun x ->
            let str = make_timestamp_string to_string x in
            w#root##.textContent := Js.some @@ Js.string str) time in
      Some w, Some s in
  let title_box = Box.make ~dir:`Column
      (title :: (Utils.List.cons_maybe subtitle [])) in
  let remove =
    let icon = Icon.SVG.(make_simple Path.close) in
    Icon_button.make ~icon () in
  let settings_icon, dialog = match item.settings with
    | None -> None, None
    | Some (settings : settings) ->
      let icon = Icon.SVG.(make_simple Path.settings) in
      let icon_button = Icon_button.make ~icon () in
      let dialog = make_dialog item settings in
      Some icon_button, Some dialog in
  let icons = match settings_icon with
    | Some icon -> [icon#widget]
    | None -> [] in
  let buttons = Card.Actions.make_icons icons in
  let actions = Card.Actions.make [buttons] in
  let heading = Box.make ~dir:`Row [title_box#widget; actions#widget] in
  let content = Card.Media.make [item.widget] in
  let widgets = [ heading#widget
                ; (Divider.make ())#widget
                ; content#widget ] in
  let elt = Card.make_element widgets in
  object(self)

    val mutable _listener = None
    val mutable _editable = false
    val mutable _removable = removable

    inherit Card.t ~widgets elt () as super

    method! init () : unit =
      super#init ();
      heading#add_class Card.CSS.primary;
      begin match settings_icon, dialog with
      | None, _ | _, None -> ()
      | Some btn, Some dlg ->
        Events.clicks btn#root (fun _ _ -> show_dialog item dlg)
        |> (fun l -> _listener <- Some l);
        Dom.appendChild Dom_html.document##.body dlg#root
      end;
      self#add_class Dashboard_tyxml.Item.CSS.root;
      title_box#add_class Dashboard_tyxml.Item.CSS.heading_title;
      content#add_class Dashboard_tyxml.Item.CSS.item_content;
      item.widget#add_class Dashboard_tyxml.Item.CSS.item_widget;
      heading#add_class Dashboard_tyxml.Item.CSS.heading;
      buttons#add_class Dashboard_tyxml.Item.CSS.heading_buttons

    method! destroy () : unit =
      super#destroy ();
      item.widget#remove_class Dashboard_tyxml.Item.CSS.item_widget;
      Utils.Option.iter Lwt.cancel _listener;
      _listener <- None;
      Utils.Option.iter Widget.destroy settings_icon;
      Utils.Option.iter
        (Widget.destroy
         % (fun x -> Dom.removeChild Dom_html.document##.body x#root; x)) dialog

    method remove = remove

    method set_removable (x : bool) =
      if not @@ _removable = x
      then if x then buttons#append_child remove
           else buttons#remove_child remove;
      _removable <- x

    method set_editable (x : bool) : unit =
      _editable <- x;
      if x && _removable
      then buttons#append_child remove
      else buttons#remove_child remove;
      item.widget#toggle_class ~force:x Dashboard_tyxml.Item.CSS.editing

  end

let make item =
  new t ~item ()
