open Containers
open Components
open Common
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types
open Widget_common

type config =
  { stream : Stream.t
  } [@@deriving yojson]

type pid_flags =
  { has_pcr : bool
  ; scrambled : bool
  } [@@deriving ord]

let name = "PIDs"

let base_class = "qos-niit-pids-overview"

let settings = None

let ( % ) = Fun.( % )

module Pid_info = struct
  type t = pid_info

  let compare (a : t) (b : t) : int =
    Int.compare a.pid b.pid
end

module Set = Set.Make(Pid_info)

let to_pid_flags { has_pcr; scrambled } =
  let pcr = match has_pcr with
    | false -> None
    | true ->
       Some Icon.SVG.(new t ~paths:Path.[ new t clock_outline () ] ()) in
  let scr = match scrambled with
    | false -> None
    | true ->
       Some Icon.SVG.(new t ~paths:Path.[ new t lock () ] ()) in
  let widgets = List.(cons_maybe pcr (cons_maybe scr [])) in
  (new Hbox.t ~widgets ())#node

let update_row row total br pid =
  let cur, per, min, max =
    let open Table in
    match row#cells with
    | _ :: _ :: _ :: _ :: a :: b :: c :: d :: _ ->
       a, b, c, d in
  let pct = 100. *. (float_of_int br)
            /. (float_of_int total) in
  let br = (float_of_int br) /. 1_000_000. in
  cur#set_value @@ Some br;
  per#set_value @@ Some pct;
  (match min#value with
   | None -> min#set_value (Some br)
   | Some v -> if br <. v then min#set_value (Some br));
  (match max#value with
   | None -> max#set_value (Some br)
   | Some v -> if br >. v then max#set_value (Some br));
  br, pct

let pid_type_to_string : pid_type -> string = function
  | SEC l ->
     let s = List.map Fun.(Mpeg_ts.(table_to_string % table_of_int)) l
             |> String.concat ", " in
     "SEC -> " ^ s
  | PES x ->
     let s = Mpeg_ts.stream_type_to_string x in
     "PES -> " ^ s
  | ECM x -> "ECM -> " ^ (string_of_int x)
  | EMM x -> "EMM -> " ^ (string_of_int x)
  | Null -> "Null"
  | Private -> "Private"

let pid_type_fmt : pid_type Table.custom =
  { to_string = pid_type_to_string
  ; compare = compare_pid_type
  ; is_numeric = false
  }

let pid_flags_fmt : pid_flags Table.custom_elt =
  { to_elt = to_pid_flags
  ; compare = compare_pid_flags
  ; is_numeric = false
  }

(* TODO add table empty state *)
let make_table (is_hex : bool)
      (init : pid_info list) =
  let open Table in
  let dec_pid_fmt = Int None in
  let hex_pid_fmt = Int (Some (Printf.sprintf "0x%04X")) in
  let br_fmt = Option (Float None, "-") in
  let pct_fmt = Option (Float (Some (Printf.sprintf "%.2f")), "-") in
  let fmt =
    let open Format in
    let to_sort_column = to_column ~sortable:true in
    (to_sort_column "PID", dec_pid_fmt)
    :: (to_sort_column "Тип", Custom pid_type_fmt)
    :: (to_column "Доп. инфо", Custom_elt pid_flags_fmt)
    :: (to_sort_column "Сервис", Option (String None, ""))
    :: (to_sort_column "Битрейт, Мбит/с", br_fmt)
    :: (to_sort_column "%", pct_fmt)
    :: (to_sort_column "Min, Мбит/с", br_fmt)
    :: (to_sort_column "Max, Мбит/с", br_fmt)
    :: [] in
  let table = new t ~dense:true ~fmt () in
  let on_change = fun (x : bool) ->
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | pid :: _ ->
           pid#set_format (if x then hex_pid_fmt else dec_pid_fmt))
      table#rows in
  if is_hex then on_change true;
  table, on_change

let add_row (table : 'a Table.t) (pid : pid_info) =
  let open Table in
  let flags =
    { has_pcr = pid.has_pcr
    ; scrambled = pid.scrambled
    } in
  let data = Data.(
      pid.pid :: pid.pid_type :: flags :: pid.service
      :: None :: None :: None :: None :: []) in
  let row = table#add_row data in
  row

class t (timestamp : Time.t option)
        (init : pid_info list)
        (control : int)
        () =
  (* FIXME should remember preffered state *)
  let is_hex = false in
  let table, on_change = make_table is_hex init in
  let title = "Список PID" in
  let subtitle = make_timestamp_string timestamp in
  let switch = new Switch.t ~state:is_hex ~on_change () in
  let hex = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let title' = new Card.Primary.title ~large:true title () in
  let subtitle' = new Card.Primary.subtitle subtitle () in
  let text_box = Widget.create_div () in
  let primary = new Card.Primary.t ~widgets:[text_box; hex#widget] () in
  let media = new Card.Media.t ~widgets:[ table ] () in
  object(self)

    val mutable _timestamp : Time.t option = timestamp
    val mutable _data : Set.t = Set.of_list init

    inherit Card.t ~widgets:[ ] ()

    (** Adds new row to the overview *)
    method add_row (x : pid_info) =
      add_row table x

    (** Updates the overview *)
    method update ({ timestamp; pids } : pids) =
      (* Update timestamp *)
      _timestamp <- Some timestamp;
      subtitle'#set_text_content @@ make_timestamp_string _timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list pids;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter prev _data in
      let upd = Set.filter (fun (x : pid_info) ->
                    List.mem ~eq:equal_pid_info x pids) inter in
      let find = fun (pid : pid_info) (row : 'a Table.Row.t) ->
        let open Table in
        let pid' = match row#cells with
          | x :: _ -> x#value in
        pid.pid = pid' in
      Set.iter (fun (pid : pid_info) ->
          match List.find_opt (find pid) table#rows with
          | None -> ()
          | Some row -> table#remove_row row) lost;
      Set.iter (fun (pid : pid_info) ->
          match List.find_opt (find pid) table#rows with
          | None -> ()
          | Some row -> self#_update_row row pid) upd;
      Set.iter (ignore % self#add_row) found

    (** Updates bitrate values *)
    method set_rate : bitrate option -> unit = function
      | None -> () (* FIXME do smth *)
      | Some { total; pids; _ } ->
         List.fold_left (fun rows (pid, br) ->
             let open Table in
             match List.find_opt (fun (row : 'a Row.t) ->
                       let cell = match row#cells with a :: _ -> a in
                       cell#value = pid) rows with
             | Some x ->
                update_row x total br pid |> ignore;
                List.remove ~eq:Equal.physical ~x rows
             | None -> rows) table#rows pids
         |> ignore

    method table = table

    method switch = hex

    method set_hex x = on_change x

    (* Private methods *)

    method private _update_row (row : 'a Table.Row.t) (x : pid_info) =
      let open Table in
      match row#cells with
      | pid :: typ :: flags :: service :: _ ->
         pid#set_value x.pid;
         typ#set_value x.pid_type;
         flags#set_value { has_pcr = x.has_pcr
                         ; scrambled = x.scrambled };
         service#set_value x.service;

    initializer
      List.iter Fun.(ignore % add_row table) init;
      text_box#append_child title';
      text_box#append_child subtitle';
      self#add_class base_class;
      self#append_child primary#widget;
      self#append_child @@ new Divider.t ();
      self#append_child media#widget;
  end

let make ?(init : (pids option, string) Lwt_result.t option)
      (stream : Stream.t)
      control =
  let init = match init with
    | Some x -> x
    | None ->
       let open Requests.Streams.HTTP in
       get_last_pids ~id:stream.id control in
  init
  >|= (function
       | None -> None, []
       | Some x -> Some x.timestamp, x.pids)
  >|= (fun (ts, data) -> new t ts data control ())
  |> Ui_templates.Loader.create_widget_loader

