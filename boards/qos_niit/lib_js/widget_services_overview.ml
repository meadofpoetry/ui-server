open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types
open Ui_templates.Sdom
open Widget_common

let ( % ) = Fun.( % )

module Service_info = struct
  type t = service_info

  let compare (a : t) (b : t) : int =
    Int.compare a.id b.id
end

module Set = Set.Make(Service_info)

let base_class = "qos-niit-services-overview"

let get_service_bitrate (br : (int * int) list) (s : service_info) =
  let ecm =
    List.fold_left (fun acc (x:ecm_info) ->
        match List.Assoc.get ~eq:(=) x.pid br with
        | None -> acc
        | Some b -> (x.pid, b) :: acc) [] s.ecm in
  let es =
    List.fold_left (fun acc (x:es_info) ->
        match List.Assoc.get ~eq:(=) x.pid br with
        | None -> acc
        | Some b -> (x.pid, b) :: acc) [] s.es in
  let pmt = match s.has_pmt with
    | false -> None
    | true -> Some (s.pmt_pid,
                    Option.get_or ~default:0
                    @@ List.Assoc.get ~eq:(=) s.pmt_pid br) in
  List.cons_maybe pmt (es @ ecm)

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

let make_table (is_hex : bool)
      (init : service_info list) =
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
      ({ id; _ } : service_info) =
  match React.S.value details with
  | Some x when x#service_id = id -> Some x
  | _ -> None

module Heading = struct

  class t ?title ?subtitle ~meta () =
    let title' = new Card.Primary.title ~large:true "" () in
    let subtitle' = new Card.Primary.subtitle "" () in
    let box = Widget.create_div ~widgets:[title'; subtitle'] () in
    object(self)
      inherit Card.Primary.t ~widgets:[box; meta#widget] ()

      method set_title (s : string) : unit =
        title'#set_text_content s

      method set_subtitle (s : string) : unit =
        subtitle'#set_text_content s

      initializer
        Option.iter self#set_title title;
        Option.iter self#set_subtitle subtitle
    end

end

let add_row (stream : Stream.t)
      (primary : Heading.t)
      (media : Card.Media.t)
      (table : 'a Table.t)
      (pids : pids option React.signal)
      (rate : bitrate option React.signal)
      (set_details : Widget_service_info.t option -> unit)
      (x : service_info)
      (control : int) =
  let row =
    table#add_row (x.id :: x.name :: x.pmt_pid :: x.pcr_pid
                   :: None :: None :: None :: None :: []) in
  row#listen_lwt Widget.Event.click (fun _ _ ->
      let open Lwt.Infix in
      let name, min, max =
        let open Table in
        match row#cells with
        | _ :: name :: _ :: _ :: _ :: _ :: b :: c :: _ ->
           name#value, b#value, c#value in
      let back = make_back () in
      primary#set_title name;
      let rate = React.S.value rate in
      let pids = React.S.value pids in
      let details =
        Widget_service_info.make ?rate ?min ?max stream x pids control in
      set_details @@ Some details;
      back#listen_once_lwt Widget.Event.click
      >|= (fun _ ->
        media#append_child table;
        media#remove_child details;
        primary#remove_child back;
        set_details None;
        details#destroy ();
        back#destroy ())
      |> Lwt.ignore_result;
      primary#insert_child_at_idx 0 back;
      media#remove_child table;
      media#append_child details;
      Lwt.return_unit)
  |> Lwt.ignore_result

class t (stream : Stream.t)
        (timestamp : Time.t option)
        (init : service_info list)
        (pids : pids option)
        (control : int)
        () =
  (* FIXME should remember previous state *)
  let is_hex = false in
  let table, on_change = make_table is_hex init in
  let title = "Список сервисов" in
  let subtitle = make_timestamp_string timestamp in
  let rate, set_rate = React.S.create None in
  let details, set_details = React.S.create None in
  let pids, set_pids = React.S.create pids in
  let on_change = fun x ->
    Option.iter (fun d -> d#set_hex x) @@ React.S.value details;
    on_change x in
  let switch = new Switch.t ~state:is_hex ~on_change () in
  let hex = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let primary = new Heading.t ~title ~subtitle ~meta:hex () in
  let media = new Card.Media.t ~widgets:[table] () in
  object(self)

    val mutable _timestamp : Time.t option = timestamp
    val mutable _data : Set.t = Set.of_list init

    inherit Card.t ~widgets:[ ] ()

    (** Adds new row to the overview *)
    method add_row (s : service_info) =
      add_row stream primary media table pids rate set_details s control

    (** Updates PID list *)
    method update_pids (pids : pids) : unit =
      set_pids @@ Some pids;
      Option.iter (fun (x : Widget_service_info.t) -> x#update_pids pids)
      @@ React.S.value details

    (** Updates the overview *)
    method update ({ timestamp; services } : services) =
      (* Update timestamp *)
      _timestamp <- Some timestamp;
      (* Set timestamp in heading only if details view is not active *)
      primary#set_subtitle @@ make_timestamp_string _timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list services;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter prev _data in
      let upd = Set.filter (fun (x : service_info) ->
                    List.mem ~eq:equal_service_info x services) inter in
      let find = fun (service : service_info) (row : 'a Table.Row.t) ->
        let open Table in
        let id = match row#cells with
          | x :: _ -> x#value in
        id = service.id in
      Set.iter (fun (info : service_info) ->
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
      Set.iter (fun (info : service_info) ->
          (* Update details, if opened *)
          begin match map_details details info with
          | Some details -> details#update info
          | None -> ()
          end;
          match List.find_opt (find info) table#rows with
          | None -> ()
          | Some row -> self#_update_row row info) upd;
      Set.iter (fun (info : service_info) ->
          (* Update details, if opened *)
          begin match map_details details info with
          | Some details ->
             details#update info;
             details#set_not_available false
          | None -> ()
          end;
          ignore @@ self#add_row info) found

    (** Updates bitrate values *)
    method set_rate (rate : bitrate option) : unit =
      set_rate rate;
      match rate with
      | None -> () (* FIXME do smth *)
      | Some rate ->
         List.iter (self#_update_row_rate rate) table#rows

    (* Private methods *)

    method private _update_row_rate (rate : bitrate) (row : 'a Table.Row.t) =
      let open Table in
      let id, cur, per, min, max = match row#cells with
        | id :: _ :: _ :: _ :: a :: b :: c :: d :: _ -> id, a, b, c, d in
      (* XXX optimize search? It is a very frequent operation *)
      match List.find_opt (fun (x : service_info) -> x.id = id#value)
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

    method private _update_row (row : 'a Table.Row.t) (service : service_info) =
      let open Table in
      match row#cells with
      | id :: name :: pmt :: pcr :: _ ->
         id#set_value service.id;
         name#set_value service.name;
         pmt#set_value service.pmt_pid;
         pcr#set_value service.pcr_pid

    initializer
      self#_keep_e
      @@ React.E.map (function
             | Some _ -> ()
             | None -> primary#set_title title)
      @@ React.S.changes details;
      List.iter Fun.(ignore % self#add_row) init;
      self#add_class base_class;
      self#append_child primary#widget;
      self#append_child @@ new Divider.t ();
      self#append_child media#widget;

  end

let lwt_l2 (a : 'a Lwt.t) (b : 'b Lwt.t) =
  a >>= fun a -> b >|= fun b -> a, b

let map_init = function
  | None -> None, []
  | Some (x : services) -> Some x.timestamp, x.services

let make ?(init : (services option, string) Lwt_result.t option)
      ?(pids : (pids option, string) Lwt_result.t option)
      (stream : Stream.t)
      (control : int) =
  let init = match init with
    | Some x -> x
    | None ->
       let open Requests.Streams.HTTP in
       get_last_services ~id:stream.id control in
  let pids = match pids with
    | Some x -> x
    | None ->
       let open Requests.Streams.HTTP in
       get_last_pids ~id:stream.id control in
  lwt_l2 (init >|= map_init) pids
  >|= (fun ((ts, data), pids) -> new t stream ts data pids control ())
  |> Ui_templates.Loader.create_widget_loader


