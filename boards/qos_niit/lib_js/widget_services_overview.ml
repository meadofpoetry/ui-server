open Containers
open Components
open Common
open Board_types
open Widget_common

let ( % ) = Fun.( % )

module Settings = struct
  type t = { hex : bool } [@@deriving eq]

  let (default : t) = { hex = false }

  let make_hex_swith state =
    let input = new Switch.t ~state () in
    new Form_field.t ~input ~align_end:true ~label:"HEX IDs" ()

  class view ?(settings = default) () =
    let hex_switch = make_hex_swith settings.hex in
    object
      val mutable value = settings
      inherit Vbox.t ~widgets:[hex_switch] ()
      method value : t = value
      method apply () : unit =
        let hex = hex_switch#input_widget#checked in
        value <- { hex }
      method reset () : unit =
        let { hex } = value in
        hex_switch#input_widget#set_checked hex
    end

  let make ?settings () = new view ?settings ()

end

module Service_info = struct
  type t = Service.t

  let compare (a : t) (b : t) : int =
    Int.compare (fst a) (fst b)
end

module Set = Set.Make(Service_info)

let base_class = "qos-niit-services-overview"
let no_sync_class = CSS.add_modifier base_class "no-sync"
let no_response_class = CSS.add_modifier base_class "no-response"

let get_service_bitrate (br : (int * int) list) ((_, info) : Service.t) =
  let elts =
    List.fold_left (fun acc pid ->
        match List.Assoc.get ~eq:(=) pid br with
        | None -> acc
        | Some b -> (pid, b) :: acc) [] info.elements in
  let pmt = match info.has_pmt with
    | false -> None
    | true -> Some (info.pmt_pid,
                    Option.get_or ~default:0
                    @@ List.Assoc.get ~eq:(=) info.pmt_pid br) in
  List.cons_maybe pmt elts

let sum_bitrate rate =
  List.fold_left (fun acc x -> acc + snd x) 0 rate

let acc_bitrate total rate =
  let sum = sum_bitrate rate in
  let pct = Float.(100. * (of_int sum / of_int total)) in
  let br_f = Float.(of_int sum / 1_000_000.) in
  sum, br_f, pct

(** Returns 'back' action element *)
let make_back () =
  let icon = Icon.SVG.(create_simple Path.arrow_left) in
  let back = Icon_button.make ~icon () in
  back#add_class @@ CSS.add_element base_class "back";
  back

let br_to_string = function
  | None -> "-"
  | Some l ->
     List.fold_left (fun acc x -> acc + snd x) 0 l
     |> (fun x -> Float.(of_int x /. 1_000_000.))
     |> Printf.sprintf "%f"

let br_fmt =
  let open Table in
  { to_string = br_to_string
  ; compare = Ord.(option @@ list @@ pair Int.compare Int.compare)
  ; is_numeric = true
  }

let min_fmt =
  let open Table in
  { to_string = br_to_string
  ; compare = (fun l1 l2 ->
    let sum = sum_bitrate in
    let cmp x1 x2 = match Int.compare (sum x1) (sum x2) with
      | -1 | 0 -> 0
      | _      -> 1 in
    (Ord.option cmp) l1 l2)
  ; is_numeric = true
  }

let max_fmt =
  let open Table in
  { to_string = br_to_string
  ; compare = (fun prev cur ->
    let sum = sum_bitrate in
    let cmp prev cur = match Int.compare (sum prev) (sum cur) with
      | 1 | 0 -> 0
      | _ -> 1 in
    (Ord.option cmp) prev cur)
  ; is_numeric = true
  }

let make_table ?(is_hex = false) () =
  let open Table in
  let hex_id_fmt = Some (Printf.sprintf "0x%04X") in
  let pct_fmt = Option (Float (Some (Printf.sprintf "%.2f")), "-") in
  let fmt =
    let open Format in
    (to_column ~sortable:true "ID", Int None)
    :: (to_column ~sortable:true "Сервис", String None)
    :: (to_column ~sortable:true "PMT PID", Int None)
    :: (to_column ~sortable:true "PCR PID", Int None)
    :: (to_column ~sortable:true "Битрейт, Мбит/с", Custom br_fmt)
    :: (to_column ~sortable:true "%", pct_fmt)
    :: (to_column ~sortable:true "Min, Мбит/с", Custom min_fmt)
    :: (to_column ~sortable:true "Max, Мбит/с", Custom max_fmt)
    :: [] in
  let table = new t ~dense:true ~fmt () in
  let on_change = fun (x : bool) ->
    List.iter (fun row ->
        match row#cells with
        | id :: _ :: pmt :: pcr :: _ ->
           let fmt = if x then Int hex_id_fmt else Int None in
           id#set_format  fmt;
           pmt#set_format fmt;
           pcr#set_format fmt)
      table#rows in
  if is_hex then on_change true;
  table, on_change

let map_details (details : Widget_service_info.t option React.signal)
      ((id, _) : Service.t) =
  match React.S.value details with
  | Some x when x#service_id = id -> Some x
  | _ -> None

let add_row (parent : #Widget.t)
      (table : 'a Table.t)
      (pids : Pid.t list Time.timestamped option React.signal)
      (rate : Bitrate.t option React.signal)
      (get_settings : unit -> Settings.t)
      (set_details : Widget_service_info.t option -> unit)
      ((id, info) : Service.t) =
  let row =
    table#push (id :: info.name :: info.pmt_pid :: info.pcr_pid
                :: None :: None :: None :: None :: []) in
  row#listen_lwt Widget.Event.click (fun _ _ ->
      let open Lwt.Infix in
      let min, max =
        let open Table in
        match row#cells with
        | _ :: _ :: _ :: _ :: _ :: _ :: b :: c :: _ ->
           b#value, c#value in
      let back = make_back () in
      let rate = React.S.value rate in
      let pids = React.S.value pids in
      let title = new Card.Primary.title info.name () in
      let primary =
        new Card.Primary.t
          ~widgets:[ back#widget; title]
          () in
      let details =
        let ({ hex } : Settings.t) = get_settings () in
        let (settings : Widget_service_info.Settings.t) = { hex } in
        Widget_service_info.make ?rate ?min ?max ~settings (id, info) pids in
      let box =
        new Vbox.t
          ~widgets:[ primary
                   ; (new Divider.t ())#widget
                   ; details#widget] () in
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

class t ?(settings : Settings.t option)
        (init : Service.t list Time.timestamped option)
        (pids : Pid.t list Time.timestamped option)
        () =
  let timestamp = match init, pids with
    | None, None -> None
    | Some { timestamp; _}, None -> Some timestamp
    | None, Some { timestamp; _ } -> Some timestamp
    | Some { timestamp = ts1; _ }, Some { timestamp = ts2; _ } ->
       if Time.is_later ts1 ~than:ts2
       then Some ts1 else Some ts2 in
  let init = match init with
    | None -> []
    | Some { data; _ } -> data in
  let s_time, set_time =
    React.S.create ~eq:(Equal.option Time.equal) timestamp in
  let table, on_change = make_table () in
  let rate, set_rate =
    React.S.create ~eq:(Equal.option Bitrate.equal) None in
  let details, set_details =
    React.S.create ~eq:(Equal.option Widget.equal) None in
  let pids, set_pids =
    let eq =
      Time.equal_timestamped (Equal.list Pid.equal)
      |> Equal.option in
    React.S.create ~eq pids in
  let empty =
    Ui_templates.Placeholder.create_with_icon
      ~icon:Icon.SVG.(create_simple Path.emoticon_sad)
      ~text:"Не найдено ни одного сервиса"
      () in
  object(self)

    val mutable _settings : Settings.t =
      Option.get_or ~default:Settings.default settings
    val mutable _data : Set.t = Set.of_list init

    inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      Option.iter self#set_settings settings;
      self#append_child table;
      React.S.map ~eq:Equal.unit (function
          | [] -> self#append_child empty
          | _ -> self#remove_child empty) table#s_rows
      |> self#_keep_s;
      List.iter Fun.(ignore % self#add_row) init;
      self#add_class base_class;

    method! destroy () : unit =
      super#destroy ();
      table#destroy ();
      empty#destroy ();
      React.S.stop ~strong:true s_time;
      React.S.stop ~strong:true rate;
      React.S.stop ~strong:true pids;
      Option.iter (fun x -> x#destroy ()) @@ React.S.value details

    method s_timestamp : Time.t option React.signal =
      s_time

    (** Adds new row to the overview *)
    method add_row (s : Service.t) =
      add_row (self :> Widget.t) table pids rate
        (fun () -> self#settings) self#set_details s

    (** Updates PID list *)
    method update_pids (pids : Pid.t list Time.timestamped) : unit =
      set_pids @@ Some pids;
      set_time @@ Some pids.timestamp;
      Option.iter (fun (x : Widget_service_info.t) -> x#update_pids pids)
      @@ React.S.value details

    (** Updates the overview *)
    method update ({ timestamp; data } : Service.t list Time.timestamped) =
      (* Update timestamp *)
      set_time @@ Some timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list data;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter _data prev in
      let upd =
        Set.filter (fun (s : Service.t) ->
            let (_, i) = Set.find s prev in
            not @@ Service.equal_info (snd s) i)
          inter in
      let find = fun ((id, _) : Service.t) (row : 'a Table.Row.t) ->
        id = Table.(match row#cells with x :: _ -> x#value) in
      Set.iter (fun (info : Service.t) ->
          (* TODO update details somehow to show that the service is lost *)
          begin match map_details details info with
          | Some details ->
             details#set_not_available true;
             details#set_rate None
          | None -> ()
          end;
          match List.find_opt (find info) table#rows with
          | None -> ()
          | Some row -> table#remove_row row) lost;
      Set.iter (fun (info : Service.t) ->
          (* Update details, if opened *)
          begin match map_details details info with
          | Some details -> details#update info
          | None -> ()
          end;
          match List.find_opt (find info) table#rows with
          | None -> ()
          | Some row -> self#_update_row row info) upd;
      Set.iter (fun (info : Service.t) ->
          (* Update details, if opened *)
          begin match map_details details info with
          | Some details ->
             details#update info;
             details#set_not_available false
          | None -> ()
          end;
          ignore @@ self#add_row info) found

    (** Updates bitrate values *)
    method set_rate (rate : Bitrate.t option) : unit =
      set_rate rate;
      match rate with
      | None -> () (* FIXME do smth *)
      | Some rate ->
         List.iter (self#_update_row_rate rate) table#rows

    (** Returns widget state *)
    method state : widget_state =
      if self#has_class no_response_class then No_response
      else if self#has_class no_sync_class then No_sync
      else Fine

    (** Updates widget state *)
    method set_state (x : widget_state) =
      begin match React.S.value details with
      | Some details -> details#set_state x
      | None -> ()
      end;
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

    method settings : Settings.t =
      _settings

    method set_settings ({ hex } as s : Settings.t) =
      _settings <- s;
      on_change hex;
      match React.S.value details with
      | None -> ()
      | Some details -> details#set_settings { hex }

    (* Private methods *)

    method private set_details (x : Widget_service_info.t option) =
      Option.iter (fun x -> x#set_state self#state) x;
      set_details x

    method private _update_row_rate (rate : Bitrate.t) (row : 'a Table.Row.t) =
      let open Table in
      let id', cur, per, min, max = match row#cells with
        | id :: _ :: _ :: _ :: a :: b :: c :: d :: _ -> id, a, b, c, d in
      (* XXX optimize search? It is a very frequent operation *)
      match List.find_opt (fun ((id, _) : Service.t) -> id = id'#value)
            @@ Set.to_list _data with
      | None -> ()
      | Some info ->
         let lst = get_service_bitrate rate.pids info in
         let _, _, pct = acc_bitrate rate.total lst in
         let details = map_details details info in
         cur#set_value @@ Some lst;
         per#set_value @@ Some pct;
         min#set_value @@ Some lst;
         max#set_value @@ Some lst;
         match details with
         | None -> ()
         | Some d -> d#set_rate @@ Some { rate with pids = lst }

    method private _update_row (row : 'a Table.Row.t) ((id, info) : Service.t) =
      Table.(match row#cells with
             | id' :: name :: pmt :: pcr :: _ ->
                id'#set_value id;
                name#set_value info.name;
                pmt#set_value info.pmt_pid;
                pcr#set_value info.pcr_pid)

  end

let make ?(settings : Settings.t option)
      (init : Service.t list Time.timestamped option)
      (pids : Pid.t list Time.timestamped option) =
  new t ?settings init pids ()

let make_dashboard_item ?settings init pids =
  let w = make ?settings init pids in
  let settings = Settings.make ?settings () in
  let (settings : Dashboard.Item.settings) =
    Dashboard.Item.make_settings
      ~widget:settings
      ~set:(fun () -> Lwt_result.return @@ w#set_settings settings#value)
      () in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let timestamp =
    Dashboard.Item.make_timestamp
      ~time:w#s_timestamp
      ~to_string:(Time.to_human_string ?tz_offset_s)
      () in
  Dashboard.Item.make_item
    ~name:"Список сервисов"
    ~subtitle:(Timestamp timestamp)
    ~settings
    w
