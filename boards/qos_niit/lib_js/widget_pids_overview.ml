open Containers
open Components
open Lwt_result.Infix
open Api_js.Api_types
open Widget_common
open Board_types
open Common

type pid_flags =
  { has_pcr : bool
  ; scrambled : bool
  } [@@deriving ord]

let name = "PIDs"

let base_class = "qos-niit-pids-overview"
let no_sync_class = Markup.CSS.add_modifier base_class "no-sync"
let no_response_class = Markup.CSS.add_modifier base_class "no-response"

let ( % ) = Fun.( % )

module Settings = struct

  type t = { hex : bool } [@@deriving eq]

  let (default : t) = { hex = false (* FIXME *) }

  class view ?(settings = default) () =
    let hex_switch =
      new Switch.t
        ~state:settings.hex
        () in
    let hex_form =
      new Form_field.t
        ~input:hex_switch
        ~align_end:true
        ~label:"HEX IDs"
        () in
    let s, set = React.S.create ~eq:equal settings in
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

module Pid_info = struct
  type t = Pid.t

  let compare (a : t) (b : t) : int =
    Int.compare (fst a) (fst b)
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

let pid_type_to_string : Pid.typ -> string = function
  | SEC l ->
     let s = List.map Fun.(Mpeg_ts.(table_to_string % table_of_int)) l
             |> String.concat ", " in
     "SEC -> " ^ s
  | PES x ->
     let s = Mpeg_ts.stream_type_to_string x.stream_type in
     "PES -> " ^ s
  | ECM x -> "ECM -> " ^ (string_of_int x.ca_sys_id)
  | EMM x -> "EMM -> " ^ (string_of_int x.ca_sys_id)
  | Null -> "Null"
  | Private -> "Private"

let pid_type_fmt : Pid.typ Table.custom =
  { to_string = pid_type_to_string
  ; compare = Pid.compare_typ
  ; is_numeric = false
  }

let pid_flags_fmt : pid_flags Table.custom_elt =
  { to_elt = to_pid_flags
  ; compare = compare_pid_flags
  ; is_numeric = false
  }

let dec_pid_fmt = Table.(Int None)
let hex_pid_fmt = Table.(Int (Some (Printf.sprintf "0x%04X")))

(* TODO add table empty state *)
let make_table_fmt ?(is_hex = false)
      (init : Pid.t list) =
  let open Table in
  let open Format in
  let br_fmt = Option (Float None, "-") in
  let pct_fmt = Option (Float (Some (Printf.sprintf "%.2f")), "-") in
  let to_sort_column = to_column ~sortable:true in
    (to_sort_column "PID", dec_pid_fmt)
    :: (to_sort_column "Тип", Custom pid_type_fmt)
    :: (to_column "Доп. инфо", Custom_elt pid_flags_fmt)
    :: (to_sort_column "Сервис", Option (String None, ""))
    :: (to_sort_column "Битрейт, Мбит/с", br_fmt)
    :: (to_sort_column "%", pct_fmt)
    :: (to_sort_column "Min, Мбит/с", br_fmt)
    :: (to_sort_column "Max, Мбит/с", br_fmt)
    :: []

let add_row (table : 'a Table.t) ((pid, info) : Pid.t) =
  let open Table in
  let flags =
    { has_pcr = info.has_pcr
    ; scrambled = info.scrambled
    } in
  let data = Data.(
      pid :: info.typ :: flags :: info.service_name
      :: None :: None :: None :: None :: []) in
  let row = table#add_row data in
  row

class t ?(settings : Settings.t option)
        (init : Pid.t list timestamped option)
        () =
  let init, timestamp = match init with
    | None -> [], None
    | Some { data; timestamp } -> data, Some timestamp in
  let s_time, set_time =
    React.S.create ~eq:(Equal.option Time.equal) timestamp in
  let fmt = make_table_fmt init in
  let table = new Table.t ~sticky_header:true ~dense:true ~fmt () in
  let empty =
    Ui_templates.Placeholder.create_with_icon
      ~icon:Icon.SVG.(create_simple Path.emoticon_sad)
      ~text:"Не найдено ни одного PID"
      () in
  object(self)

    val mutable _data : Set.t = Set.of_list init

    inherit Widget.t Dom_html.(createDiv document) () as super

    method init () : unit =
      super#init ();
      Option.iter self#set_settings settings;
      self#append_child table;
      React.S.map ~eq:Equal.unit (function
          | [] -> self#append_child empty
          | _ -> self#remove_child empty) table#s_rows
      |> self#_keep_s;
      List.iter Fun.(ignore % add_row table) init;
      self#add_class base_class

    method destroy () : unit =
      super#destroy ();
      table#destroy ();
      empty#destroy ();

    method pids = Set.to_list _data

    method set_settings ({ hex } : Settings.t) =
      self#set_hex hex

    method s_timestamp : Time.t option React.signal =
      s_time

    (** Updates widget state *)
    method set_state = function
      | Fine ->
         self#remove_class no_response_class;
         self#remove_class no_sync_class
      | No_sync ->
         self#remove_class no_response_class;
         self#add_class no_sync_class
      | No_response ->
         self#remove_class no_sync_class;
         self#add_class no_response_class

    (** Adds new row to the overview *)
    method add_pid (x : Pid.t) =
      add_row table x

    (** Updates the overview *)
    method update ({ timestamp; data } : Pid.t list timestamped) =
      set_time @@ Some timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list data;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter prev _data in
      let upd =
        Set.filter (fun ((_, info) : Pid.t) ->
            List.mem ~eq:Pid.equal_info info @@ List.map snd data) inter in
      let find = fun ((pid, _) : Pid.t) (row : 'a Table.Row.t) ->
        let open Table in
        let pid' = match row#cells with
          | x :: _ -> x#value in
        pid = pid' in
      Set.iter (fun (pid : Pid.t) ->
          match List.find_opt (find pid) table#rows with
          | None -> ()
          | Some row -> table#remove_row row) lost;
      Set.iter (fun (pid : Pid.t) ->
          match List.find_opt (find pid) table#rows with
          | None -> ()
          | Some row -> self#_update_row row pid) upd;
      Set.iter (ignore % self#add_pid) found

    (** Updates bitrate values *)
    method set_rate : Bitrate.t option -> unit = function
      | None -> () (* FIXME do smth *)
      | Some { total; pids; _ } ->
         List.fold_left (fun rows (pid, br) ->
             let open Table in
             match List.find_opt (fun (row : 'a Row.t) ->
                       let cell = match row#cells with a :: _ -> a in
                       cell#value = pid) rows with
             | Some x ->
                update_row x total br pid |> ignore;
                List.remove ~eq:Equal.physical x rows
             | None -> rows) table#rows pids
         |> ignore

    (** Sets ID display format to dec or hex *)
    method set_hex (x : bool) : unit =
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | pid :: _ ->
           pid#set_format (if x then hex_pid_fmt else dec_pid_fmt))
      table#rows

    (* Private methods *)

    method private _update_row (row : 'a Table.Row.t) ((pid, info) : Pid.t) =
      let open Table in
      match row#cells with
      | pid' :: typ :: flags :: service :: _ ->
         pid'#set_value pid;
         typ#set_value info.typ;
         flags#set_value { has_pcr = info.has_pcr
                         ; scrambled = info.scrambled };
         service#set_value info.service_name;

  end

let make ?settings
      (init : Pid.t list timestamped option) =
  new t ?settings init ()

let make_dashboard_item ?settings init : 'a Dashboard.Item.item =
  let w = make ?settings init in
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
