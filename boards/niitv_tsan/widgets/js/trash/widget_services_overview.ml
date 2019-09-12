open Containers
open Components
open Common
open Board_types
open Widget_common

(** Returns 'back' action element *)
let make_back () =
  let icon = Icon.SVG.(create_simple Path.arrow_left) in
  let back = new Icon_button.t ~icon () in
  back#add_class @@ Markup.CSS.add_element base_class "back";
  back

let map_details
    (details : Widget_service_info.t option React.signal)
    ((id, _) : Service.t) =
  match React.S.value details with
  | Some x when x#service_id = id -> Some x
  | _ -> None

let add_row
    (parent : #Widget.t)
    (table : 'a Table.t)
    (pids : Pid.t list Time.timestamped option React.signal)
    (rate : Bitrate.t option React.signal)
    (get_settings : unit -> Settings.t)
    (set_details : Widget_service_info.t option -> unit)
    ((id, info) : Service.t) =
  let row =
    table#push [id; info.name; info.pmt_pid; info.pcr_pid; None; None; None; None]
  in
  row#listen_lwt Widget.Event.click (fun _ _ ->
      let open Lwt.Infix in
      let min, max =
        let open Table in
        match row#cells with
        | _ :: _ :: _ :: _ :: _ :: _ :: b :: c :: _ -> b#value, c#value
      in
      let back = make_back () in
      let rate = React.S.value rate in
      let pids = React.S.value pids in
      let title = new Card.Primary.title info.name () in
      let primary = new Card.Primary.t ~widgets:[back#widget; title] () in
      let details =
        let ({hex} : Settings.t) = get_settings () in
        let (settings : Widget_service_info.Settings.t) = {hex} in
        Widget_service_info.make ?rate ?min ?max ~settings (id, info) pids
      in
      let box =
        new Vbox.t ~widgets:[primary; (new Divider.t ())#widget; details#widget] ()
      in
      set_details @@ Some details;
      back#listen_once_lwt Widget.Event.click
      >|= (fun _ ->
            parent#append_child table;
            parent#remove_child box;
            set_details None;
            details#destroy ();
            back#destroy ())
      |> Lwt.ignore_result;
      parent#remove_child table;
      parent#append_child box;
      Lwt.return_unit)
  |> Lwt.ignore_result
