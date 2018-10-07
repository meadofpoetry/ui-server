open Containers
open Components
open Common
open Board_types
open Lwt_result.Infix
open Api_js.Api_types
open Ui_templates.Sdom
open Widget_common

let ( % ) = Fun.( % )

module Settings = struct

  type t = { hex : bool }

  let (default : t) = { hex = false (* FIXME *) }

  class view ?(settings = default) () =
    let hex_switch =
      new Switch.t
        ~state:settings.hex
        () in
    let hex_form =
      new Form_field.t
        ~align_end:true
        ~input:hex_switch
        ~label:"HEX IDs"
        () in
    let s, set = React.S.create settings in
    object(self)

      inherit Vbox.t ~widgets:[hex_form] ()

      method apply () : unit =
        let hex = hex_switch#checked in
        set { hex }

      method reset () : unit =
        let { hex } = React.S.value self#s in
        hex_switch#set_checked hex

      method s : t React.signal = s

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
let no_sync_class = Markup.CSS.add_modifier base_class "no-sync"
let no_response_class = Markup.CSS.add_modifier base_class "no-response"

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
  let back = new Icon_button.t ~icon () in
  back#add_class @@ Markup.CSS.add_element base_class "back";
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

let make_table ?(is_hex = false)
      (init : Service.t list) =
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
        let open Table in
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
      (pids : Pid.t list timestamped option React.signal)
      (rate : Bitrate.t option React.signal)
      (set_details : Widget_service_info.t option -> unit)
      ((id, info) : Service.t) =
  let row =
    table#add_row (id :: info.name :: info.pmt_pid :: info.pcr_pid
                   :: None :: None :: None :: None :: []) in
  row#listen_lwt Widget.Event.click (fun _ _ ->
      let open Lwt.Infix in
      let name, min, max =
        let open Table in
        match row#cells with
        | _ :: name :: _ :: _ :: _ :: _ :: b :: c :: _ ->
           name#value, b#value, c#value in
      let back = make_back () in
      let rate = React.S.value rate in
      let pids = React.S.value pids in
      let title = new Card.Primary.title info.name () in
      let primary =
        new Card.Primary.t
          ~widgets:[ back#widget; title]
          () in
      let details =
        Widget_service_info.make ?rate ?min ?max (id, info) pids in
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
        (init : Service.t list timestamped option)
        (pids : Pid.t list timestamped option)
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
  let s_time, set_time = React.S.create timestamp in
  let table, on_change = make_table init in
  let rate, set_rate = React.S.create None in
  let details, set_details = React.S.create None in
  let pids, set_pids = React.S.create pids in
  let empty =
    Ui_templates.Placeholder.create_with_icon
      ~icon:Icon.SVG.(create_simple Path.emoticon_sad)
      ~text:"Не найдено ни одного сервиса"
      () in
  object(self)

    val mutable _data : Set.t = Set.of_list init

    inherit Widget.t Dom_html.(createDiv document) ()

    method s_timestamp : Time.t option React.signal =
      s_time

    (** Adds new row to the overview *)
    method add_row (s : Service.t) =
      add_row (self :> Widget.t) table pids rate self#set_details s

    (** Updates PID list *)
    method update_pids (pids : Pid.t list timestamped) : unit =
      set_pids @@ Some pids;
      set_time @@ Some pids.timestamp;
      Option.iter (fun (x : Widget_service_info.t) -> x#update_pids pids)
      @@ React.S.value details

    (** Updates the overview *)
    method update ({ timestamp; data } : Service.t list timestamped) =
      (* Update timestamp *)
      set_time @@ Some timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list data;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter prev _data in
      let upd =
        Set.filter (fun ((_, info) : Service.t) ->
            List.mem ~eq:Service.equal_info info @@ List.map snd data) inter in
      let find = fun ((id, _) : Service.t) (row : 'a Table.Row.t) ->
        let open Table in
        let id' = match row#cells with
          | x :: _ -> x#value in
        id' = id in
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

    (** Sets identifiers to hex or decimal view *)
    method set_hex (x : bool) : unit =
      Option.iter (fun d -> d#set_hex x) @@ React.S.value details;
      on_change x

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

    method set_settings (x : Settings.t) =
      self#set_hex x.hex

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
         let open Option in
         let lst = get_service_bitrate rate.pids info in
         let br, br_f, pct = acc_bitrate rate.total lst in
         let details = map_details details info in
         cur#set_value @@ Some lst;
         per#set_value @@ Some pct;
         min#set_value @@ Some lst;
         max#set_value @@ Some lst;
         iter (fun x -> x#set_rate @@ Some { rate with pids = lst }) details

    method private _update_row (row : 'a Table.Row.t) ((id, info) : Service.t) =
      let open Table in
      match row#cells with
      | id' :: name :: pmt :: pcr :: _ ->
         id'#set_value id;
         name#set_value info.name;
         pmt#set_value info.pmt_pid;
         pcr#set_value info.pcr_pid

    initializer
      Option.iter self#set_settings settings;
      self#append_child table;
      React.S.map (function
          | [] -> self#append_child empty
          | _ -> self#remove_child empty) table#s_rows
      |> self#_keep_s;
      List.iter Fun.(ignore % self#add_row) init;
      self#add_class base_class;

  end

let make ?(settings : Settings.t option)
      (init : Service.t list timestamped option)
      (pids : Pid.t list timestamped option) =
  new t ?settings init pids ()

let make_dashboard_item ?settings init pids =
  let w = make ?settings init pids in
  let settings = Settings.make ?settings () in
  let s = settings#s in
  let (settings : Dashboard.Item.settings) =
    { widget = settings#widget
    ; ready = React.S.const true
    ; set = (fun () -> Lwt_result.return @@ w#set_settings @@ React.S.value s)
    } in
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let timestamp =
    Dashboard.Item.make_timestamp
      ~time:w#s_timestamp
      ~to_string:(Time.to_human_string ?tz_offset_s)
      () in
  Dashboard.Item.make_item
    ~name:"Обзор"
    ~subtitle:(Timestamp timestamp)
    ~settings
    w
