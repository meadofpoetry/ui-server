open Containers
open Components
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

  type t = { hex : bool }

  let (default : t) = { hex = false }

  let make_hex_switch state =
    let input = new Switch.t ~state () in
    new Form_field.t ~input ~align_end:true ~label:"HEX IDs" ()

  class view ?(settings = default) () =
    let hex_switch = make_hex_switch settings.hex in
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
       Some Icon.SVG.(new t ~paths:Path.[new t lock ()] ()) in
  let widgets = List.(cons_maybe pcr (cons_maybe scr [])) in
  (new Hbox.t ~widgets ())#node

let update_row row total br =
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

let pid_type_fmt : Mpeg_ts.Pid.Type.t Table.custom =
  Mpeg_ts.Pid.Type.{ to_string
                   ; compare
                   ; is_numeric = false }

let pid_flags_fmt : pid_flags Table.custom_elt =
  { to_elt = to_pid_flags
  ; compare = compare_pid_flags
  ; is_numeric = false
  }

let dec_pid_fmt = Table.(Int None)
let hex_pid_fmt = Table.(Int (Some (Printf.sprintf "0x%04X")))

(* TODO add table empty state *)
let make_table_fmt ?(is_hex = false) () =
  let open Table in
  let open Format in
  let br_fmt = Option (Float None, "-") in
  let pct_fmt = Option (Float (Some (Printf.sprintf "%.2f")), "-") in
  let to_sort_column = to_column ~sortable:true in
    (to_sort_column "PID", if is_hex then hex_pid_fmt else dec_pid_fmt)
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
  let row = table#push data in
  row

class t ?(settings : Settings.t option)
        (init : Pid.t list Time.timestamped option)
        () =
  let init, timestamp = match init with
    | None -> [], None
    | Some { data; timestamp } -> data, Some timestamp in
  let s_time, set_time =
    React.S.create ~eq:(Equal.option Time.equal) timestamp in
  let is_hex = Option.map (fun (x : Settings.t) -> x.hex) settings in
  let fmt = make_table_fmt ?is_hex () in
  let table = new Table.t ~sticky_header:true ~dense:true ~fmt () in
  let empty =
    Ui_templates.Placeholder.create_with_icon
      ~icon:Icon.SVG.(create_simple Path.emoticon_sad)
      ~text:"Не найдено ни одного PID"
      () in
  object(self)

    val mutable _settings : Settings.t =
      Option.get_or ~default:Settings.default settings
    val mutable _data : Set.t = Set.of_list init

    inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) () as super

    method! init () : unit =
      super#init ();
      self#append_child table;
      React.S.map ~eq:Equal.unit (function
          | [] -> self#append_child empty
          | _ -> self#remove_child empty) table#s_rows
      |> self#_keep_s;
      List.iter Fun.(ignore % add_row table) init;
      Option.iter self#set_settings settings;
      self#add_class base_class

    method! destroy () : unit =
      super#destroy ();
      table#destroy ();
      empty#destroy ();

    method pids = Set.to_list _data

    method settings : Settings.t = _settings

    method set_settings ({ hex } as s : Settings.t) =
      _settings <- s;
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
    method update ({ timestamp; data } : Pid.t list Time.timestamped) =
      set_time @@ Some timestamp;
      (* Manage found, lost and updated items *)
      let prev = _data in
      _data <- Set.of_list data;
      let lost = Set.diff prev _data in
      let found = Set.diff _data prev in
      let inter = Set.inter _data prev in
      let upd =
        Set.filter (fun (p : Pid.t) ->
            let (_, i) = Set.find p prev in
            not @@ Pid.equal_info (snd p) i)
          inter in
      let find = fun ((pid, _) : Pid.t) (row : 'a Table.Row.t) ->
        pid = Table.(match row#cells with x :: _ -> x#value) in
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
                ignore @@ update_row x total br;
                List.remove ~eq:Widget.equal x rows
             | None -> rows) table#rows pids
         |> ignore

    (* Private methods *)

    method private set_hex (x : bool) : unit =
      let fmt = if x then hex_pid_fmt else dec_pid_fmt in
      let iter = function
        | Table.(pid :: _) -> pid#set_format fmt in
      List.iter (fun row -> iter row#cells) table#rows

    method private _update_row (row : 'a Table.Row.t) ((pid, info) : Pid.t) =
      Table.(match row#cells with
             | pid' :: typ :: flags :: service :: _ ->
                pid'#set_value pid;
                typ#set_value info.typ;
                flags#set_value { has_pcr = info.has_pcr
                                ; scrambled = info.scrambled };
                service#set_value info.service_name)

  end

let make ?settings
      (init : Pid.t list Time.timestamped option) =
  new t ?settings init ()

let make_dashboard_item ?settings init : 'a Dashboard.Item.item =
  let w = make ?settings init in
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
    ~name:"Список PID"
    ~subtitle:(Timestamp timestamp)
    ~settings
    w
