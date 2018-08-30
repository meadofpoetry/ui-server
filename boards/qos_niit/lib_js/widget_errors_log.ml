open Containers
open Components
open Common
open Board_types
open Board_types.Streams.TS
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  }

let name = "Errors"

let base_class = "qos-niit-errors-log"

let settings = None

let ( >>* ) x f = Lwt_result.map_err f x
let ( % ) = Fun.( % )

let get_errors ?from ?till ?duration ?limit ?order ~id control =
  let open Requests.Streams.HTTP.Errors in
  get_errors ?limit ?from ?till ?duration ?order ~id control
  >>* Api_js.Requests.err_to_string
  >>= function
  | Raw s -> Lwt_result.return s.data
  | _ -> Lwt.fail_with "got compressed"

let pid_fmt hex =
  let open Table in
  let to_string = fun (pid, multi) ->
    let f =
      if hex then Printf.sprintf "0x%04X"
      else Printf.sprintf "%d" in
    let s = f pid in
    if multi then s ^ "*" else s in
  let compare = fun (pid1, _) (pid2, _) ->
    Int.compare pid1 pid2 in
  Custom { to_string; compare; is_numeric = true }

let add_error (table : 'a Table.t) (error : Errors.t) =
  let open Table in
  let service = None in
  let date = error.timestamp in
  let pid = error.pid, error.multi_pid in
  let count = error.count in
  let check =
    let num, name = Ts_error.to_name error in
    num ^ " " ^ name in
  let extra = Ts_error.Description.of_ts_error error in
  let data = Data.(
      date :: check :: pid :: service :: count :: extra :: []) in
  let row = table#prepend_row data in
  row

let make_table ~id is_hex (init : Errors.raw) control =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let show_time = Time.to_human_string ?tz_offset_s in
  let fmt =
    let open Table in
    let open Format in
    (to_column ~sortable:true "Время", Time (Some show_time))
    :: (to_column ~sortable:true "Событие", String None)
    :: (to_column ~sortable:true "PID", pid_fmt false)
    :: (to_column ~sortable:true "Сервис", Option (String None, ""))
    :: (to_column ~sortable:true "Количество", Int None)
    :: (to_column "Подробности", String None)
    :: [] in
  let fwd =
    let icon = Icon.SVG.(create_simple Path.chevron_right) in
    new Icon_button.t ~icon () in
  let bwd =
    let icon = Icon.SVG.(create_simple Path.chevron_left) in
    new Icon_button.t ~icon () in
  let fst =
    let icon = Icon.SVG.(create_simple Path.page_first) in
    new Icon_button.t ~icon () in
  let lst =
    let icon = Icon.SVG.(create_simple Path.page_last) in
    new Icon_button.t ~icon () in
  let select = new Table.Footer.Select.t
                 [ 5; 10; 15; 20 ] () in
  let footer =
    new Table.Footer.t
      ~actions:[ fst; bwd; fwd; lst ]
      ~rows_per_page:("Ошибок на странице: ", select) () in
  let table = new Table.t ~footer ~dense:true ~fmt () in
  fst#listen_click_lwt (fun _ _ ->
      get_errors ~limit:20 ~order:`Desc ~id control
      >|= (fun e -> table#remove_all_rows ();
                    List.iter (ignore % add_error table % snd) e)
      |> Lwt.map ignore)
  |> Lwt.ignore_result;
  fwd#listen_click_lwt (fun _ _ ->
      let open Table in
      let till =
        List.fold_left (fun acc x ->
            let cell = match x#cells with
              | c :: _ -> c in
            match acc with
            | None -> Some cell#value
            | Some acc ->
               if Time.compare cell#value acc >= 0
               then Some acc else Some cell#value)
          None table#rows in
      print_endline @@ show_time @@ Option.get_exn till;
      get_errors ?till ~limit:20 ~order:`Desc ~id control
      >|= (fun e -> table#remove_all_rows ();
                    List.iter (ignore % add_error table % snd) e)
      |> Lwt.map ignore)
  |> Lwt.ignore_result;
  bwd#listen_click_lwt (fun _ _ ->
      let open Table in
      let from =
        List.fold_left (fun acc x ->
            let cell = match x#cells with
              | c :: _ -> c in
            match acc with
            | None -> Some cell#value
            | Some acc ->
               if Time.compare cell#value acc < 0
               then Some acc else Some cell#value)
          None table#rows in
      print_endline @@ show_time @@ Option.get_exn from;
      get_errors ?from ~limit:20 ~order:`Asc ~id control
      >|= (fun e -> table#remove_all_rows ();
                    List.iter (ignore % add_error table % snd) @@ List.rev e)
      |> Lwt.map ignore)
  |> Lwt.ignore_result;
  lst#listen_click_lwt (fun _ _ ->
      get_errors ~limit:20 ~order:`Asc ~id control
      >|= (fun e -> table#remove_all_rows ();
                    List.iter (ignore % add_error table % snd) @@ List.rev e)
      |> Lwt.map ignore)
  |> Lwt.ignore_result;
  let on_change = fun (x : bool) ->
    List.iter (fun row ->
        let open Table in
        match row#cells with
        | _ :: _ :: pid :: _ ->
           let fmt = pid_fmt x in
           pid#set_format fmt)
      table#rows in
  if is_hex then on_change true;
  table, on_change, add_error

class t ~id (init : Errors.raw) control () =
  (* FIXME should remember preffered state *)
  let is_hex = false in
  let table, on_change, add_row = make_table ~id is_hex init control in
  let actions = new Card.Actions.t ~widgets:[] () in
  let media = new Card.Media.t ~widgets:[ table ] () in
  let switch = new Switch.t ~state:is_hex ~on_change () in
  let hex = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  object(self)

    inherit Card.t ~widgets:[ actions#widget
                            ; (new Divider.t ())#widget
                            ; media#widget ] ()

    method add_error : Errors.t -> 'a Table.Row.t =
      add_row table

    initializer
      List.iter Fun.(ignore % self#add_error % snd) init;
      actions#append_child hex;
      self#add_class base_class
  end

let make ~id init control = new t ~id init control ()
