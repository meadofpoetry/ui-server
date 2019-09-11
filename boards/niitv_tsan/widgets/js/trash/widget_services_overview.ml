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

let make_table ?(is_hex = false) () =
  let open Table in
  let table = new t ~dense:true ~fmt () in
  let on_change (x : bool) =
    List.iter
      (fun row ->
        match row#cells with
        | id :: _ :: pmt :: pcr :: _ ->
            let fmt = if x then Int hex_id_fmt else Int None in
            id#set_format fmt;
            pmt#set_format fmt;
            pcr#set_format fmt)
      table#rows
  in
  if is_hex then on_change true;
  table, on_change

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

class t
  ?(settings : Settings.t option)
  (init : Service.t list Time.timestamped option)
  (pids : Pid.t list Time.timestamped option)
  () =
  let timestamp =
    match init, pids with
    | None, None -> None
    | Some {timestamp; _}, None -> Some timestamp
    | None, Some {timestamp; _} -> Some timestamp
    | Some {timestamp = ts1; _}, Some {timestamp = ts2; _} ->
        if Time.is_later ts1 ~than:ts2 then Some ts1 else Some ts2
  in
  let init =
    match init with
    | None -> []
    | Some {data; _} -> data
  in
  let s_time, set_time = React.S.create ~eq:(Equal.option Time.equal) timestamp in
  let table, on_change = make_table () in
  let rate, set_rate = React.S.create ~eq:(Equal.option Bitrate.equal) None in
  let details, set_details = React.S.create ~eq:(Equal.option Widget.equal) None in
  let pids, set_pids =
    let eq = Time.equal_timestamped (Equal.list Pid.equal) |> Equal.option in
    React.S.create ~eq pids
  in
  let empty =
    Ui_templates.Placeholder.create_with_icon
      ~icon:Icon.SVG.(create_simple Path.emoticon_sad)
      ~text:"Не найдено ни одного сервиса"
      ()
  in
  object (self)
    inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      Option.iter self#set_settings settings;
      self#append_child table;
      React.S.map
        ~eq:Equal.unit
        (function
          | [] -> self#append_child empty
          | _ -> self#remove_child empty)
        table#s_rows
      |> self#_keep_s;
      List.iter Fun.(ignore % self#add_row) init;
      self#add_class base_class

    method add_row (s : Service.t) =
      add_row
        (self :> Widget.t)
        table
        pids
        rate
        (fun () -> self#settings)
        self#set_details
        s
    (** Adds new row to the overview *)

    method update_pids (pids : Pid.t list Time.timestamped) : unit =
      set_pids @@ Some pids;
      set_time @@ Some pids.timestamp;
      Option.iter (fun (x : Widget_service_info.t) -> x#update_pids pids)
      @@ React.S.value details
    (** Updates PID list *)

    method update ({timestamp; data} : Service.t list Time.timestamped) =
      (* Update timestamp *)
      set_time @@ Some timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list data;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter _data prev in
      let upd =
        Set.filter
          (fun (s : Service.t) ->
            let _, i = Set.find s prev in
            not @@ Service.equal_info (snd s) i)
          inter
      in
      let find ((id, _) : Service.t) (row : 'a Table.Row.t) =
        id
        = Table.(
            match row#cells with
            | x :: _ -> x#value)
      in
      Set.iter
        (fun (info : Service.t) ->
          (* TODO update details somehow to show that the service is lost *)
          (match map_details details info with
          | Some details ->
              details#set_not_available true;
              details#set_rate None
          | None -> ());
          match List.find_opt (find info) table#rows with
          | None -> ()
          | Some row -> table#remove_row row)
        lost;
      Set.iter
        (fun (info : Service.t) ->
          (* Update details, if opened *)
          (match map_details details info with
          | Some details -> details#update info
          | None -> ());
          match List.find_opt (find info) table#rows with
          | None -> ()
          | Some row -> self#_update_row row info)
        upd;
      Set.iter
        (fun (info : Service.t) ->
          (* Update details, if opened *)
          (match map_details details info with
          | Some details ->
              details#update info;
              details#set_not_available false
          | None -> ());
          ignore @@ self#add_row info)
        found
    (** Updates the overview *)

    method set_rate (rate : Bitrate.t option) : unit =
      set_rate rate;
      match rate with
      | None -> () (* FIXME do smth *)
      | Some rate -> List.iter (self#_update_row_rate rate) table#rows
    (** Updates bitrate values *)

    method state : widget_state =
      if self#has_class no_response_class
      then No_response
      else if self#has_class no_sync_class
      then No_sync
      else Fine
    (** Returns widget state *)

    method set_state (x : widget_state) =
      (match React.S.value details with
      | Some details -> details#set_state x
      | None -> ());
      match x with
      | Fine ->
          self#remove_class no_response_class;
          self#remove_class no_sync_class
      | No_sync ->
          self#remove_class no_response_class;
          self#add_class no_sync_class
      | No_response ->
          self#remove_class no_sync_class;
          self#add_class no_response_class
    (** Updates widget state *)

    (* Private methods *)
    method private set_details (x : Widget_service_info.t option) =
      Option.iter (fun x -> x#set_state self#state) x;
      set_details x

    method private _update_row_rate (rate : Bitrate.t) (row : 'a Table.Row.t) =
      let open Table in
      let id', cur, per, min, max =
        match row#cells with
        | id :: _ :: _ :: _ :: a :: b :: c :: d :: _ -> id, a, b, c, d
      in
      (* XXX optimize search? It is a very frequent operation *)
      match
        List.find_opt (fun ((id, _) : Service.t) -> id = id'#value) @@ Set.to_list _data
      with
      | None -> ()
      | Some info -> (
          let lst = get_service_bitrate rate.pids info in
          let _, _, pct = acc_bitrate rate.total lst in
          let details = map_details details info in
          cur#set_value @@ Some lst;
          per#set_value @@ Some pct;
          min#set_value @@ Some lst;
          max#set_value @@ Some lst;
          match details with
          | None -> ()
          | Some d -> d#set_rate @@ Some {rate with pids = lst})

    method private _update_row (row : 'a Table.Row.t) ((id, info) : Service.t) =
      Table.(
        match row#cells with
        | id' :: name :: pmt :: pcr :: _ ->
            id'#set_value id;
            name#set_value info.name;
            pmt#set_value info.pmt_pid;
            pcr#set_value info.pcr_pid)
  end
