open Containers
open Components
open Common
open Lwt_result.Infix
open Api_js.Api_types
open Widget_common
open Board_types

type pid_flags =
  { has_pcr : bool
  ; scrambled : bool
  } [@@deriving ord]

let name = "PIDs"

let base_class = "qos-niit-pids-overview"

let ( % ) = Fun.( % )

module Settings = struct

  type t =
    { hex : bool
    }

  let (default : t) =
    { hex = false (* FIXME *)
    }

  class view () =
    let hex_switch =
      new Switch.t
        ~state:default.hex
        () in
    let hex_form =
      new Form_field.t
        ~input:hex_switch
        ~label:"HEX IDs"
        () in
    let s, set = React.S.create default in
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

  let make () = new view ()

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
let make_table_fmt (is_hex : bool)
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

class ['a] t (init : Pid.t list timestamped option) () =
  let settings = Settings.make () in
  let init, timestamp = match init with
    | None -> [], None
    | Some { data; timestamp } -> data, Some timestamp in
  let is_hex = false in
  let fmt = make_table_fmt is_hex init in
  object(self)

    val mutable _timestamp : Time.t option = timestamp
    val mutable _data : Set.t = Set.of_list init

    inherit ['a] Table.t ~sticky_header:true
              ~dense:true ~fmt ()

    method settings_widget = settings

    (** Adds new row to the overview *)
    method add_pid (x : Pid.t) =
      add_row (self :> 'a Table.t) x

    (** Updates the overview *)
    method update ({ timestamp; data } : Pid.t list timestamped) =
      _timestamp <- Some timestamp;
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
          match List.find_opt (find pid) self#rows with
          | None -> ()
          | Some row -> self#remove_row row) lost;
      Set.iter (fun (pid : Pid.t) ->
          match List.find_opt (find pid) self#rows with
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
                List.remove ~eq:Equal.physical ~x rows
             | None -> rows) self#rows pids
         |> ignore

    method pids = Set.to_list _data

    method set_hex (x : bool) : unit =
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | pid :: _ ->
           pid#set_format (if x then hex_pid_fmt else dec_pid_fmt))
      self#rows

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

    initializer
      List.iter Fun.(ignore % add_row (self :> 'a Table.t)) init;
      self#add_class base_class
  end

let make ?(init : (pids, string) Lwt_result.t option)
      (stream : Stream.ID.t)
      control =
  let init = match init with
    | Some x -> x
    | None ->
       let open Requests.Streams.HTTP in
       get_pids ~ids:[stream] control
       |> Lwt_result.map_err Api_js.Requests.err_to_string in
  init
  >|= (function
       | [(_, x)] -> new t (Some x) ()
       | _ -> new t None ()) (* FIXME show error *)

