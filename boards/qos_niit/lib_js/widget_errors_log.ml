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
  | Raw s -> Lwt_result.return (s.has_more, s.data)
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

let make_row_data (error : Errors.t) =
  let open Table in
  let service = None in
  let date = error.timestamp in
  let pid = error.pid, error.multi_pid in
  let count = error.count in
  let check =
    let num, name = Ts_error.to_name error in
    num ^ " " ^ name in
  let extra = Ts_error.Description.of_ts_error error in
  Data.(date :: check :: pid :: service :: count :: extra :: [])

let make_table ~id is_hex (init : Errors.raw) control =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let show_time = Time.to_human_string ?tz_offset_s in
  let fmt =
    let sortable = false in
    let open Table in
    let open Format in
    (to_column ~sortable "Время", Time (Some show_time))
    :: (to_column ~sortable "Событие", String None)
    :: (to_column ~sortable "PID", pid_fmt false)
    :: (to_column ~sortable "Сервис", Option (String None, ""))
    :: (to_column ~sortable "Количество", Int None)
    :: (to_column "Подробности", String None)
    :: [] in
  (* let fwd =
   *   let icon = Icon.SVG.(create_simple Path.chevron_right) in
   *   new Icon_button.t ~icon () in
   * let bwd =
   *   let icon = Icon.SVG.(create_simple Path.chevron_left) in
   *   new Icon_button.t ~icon () in
   * let fst =
   *   let icon = Icon.SVG.(create_simple Path.page_first) in
   *   new Icon_button.t ~icon () in
   * let lst =
   *   let icon = Icon.SVG.(create_simple Path.page_last) in
   *   new Icon_button.t ~icon () in
   * let select = new Table.Footer.Select.t
   *                [ 5; 10; 15; 20 ] () in
   * let footer =
   *   new Table.Footer.t
   *     ~actions:[ fst; bwd; fwd; lst ]
   *     ~rows_per_page:("Ошибок на странице: ", select) () in
   * let table = new Table.t ~footer ~dense:true ~fmt () in
   * fst#listen_click_lwt (fun _ _ ->
   *     get_errors ~limit:200 ~order:`Desc ~id control
   *     >|= (fun e -> table#remove_all_rows ();
   *                   List.iter (ignore % add_error table % snd) e)
   *     |> Lwt.map ignore)
   * |> Lwt.ignore_result;
   * fwd#listen_click_lwt (fun _ _ ->
   *     let open Table in
   *     let till =
   *       List.fold_left (fun acc x ->
   *           let cell = match x#cells with
   *             | c :: _ -> c in
   *           match acc with
   *           | None -> Some cell#value
   *           | Some acc ->
   *              if Time.compare cell#value acc >= 0
   *              then Some acc else Some cell#value)
   *         None table#rows in
   *     print_endline @@ show_time @@ Option.get_exn till;
   *     get_errors ?till ~limit:20 ~order:`Desc ~id control
   *     >|= (fun e -> table#remove_all_rows ();
   *                   List.iter (ignore % add_error table % snd) e)
   *     |> Lwt.map ignore)
   * |> Lwt.ignore_result;
   * bwd#listen_click_lwt (fun _ _ ->
   *     let open Table in
   *     let from =
   *       List.fold_left (fun acc x ->
   *           let cell = match x#cells with
   *             | c :: _ -> c in
   *           match acc with
   *           | None -> Some cell#value
   *           | Some acc ->
   *              if Time.compare cell#value acc < 0
   *              then Some acc else Some cell#value)
   *         None table#rows in
   *     print_endline @@ show_time @@ Option.get_exn from;
   *     get_errors ?from ~limit:20 ~order:`Asc ~id control
   *     >|= (fun e -> table#remove_all_rows ();
   *                   List.iter (ignore % add_error table % snd) @@ List.rev e)
   *     |> Lwt.map ignore)
   * |> Lwt.ignore_result;
   * lst#listen_click_lwt (fun _ _ ->
   *     get_errors ~limit:20 ~order:`Asc ~id control
   *     >|= (fun e -> table#remove_all_rows ();
   *                   List.iter (ignore % add_error table % snd) @@ List.rev e)
   *     |> Lwt.map ignore)
   * |> Lwt.ignore_result; *)
  let table =
    new Table.t
      ~sticky_header:true
      ~dense:true
      ~fmt () in
  let on_change = fun (x : bool) ->
    let fmt =
      let open Table.Format in
      match fmt with
      | a :: b :: (c, _) :: tl -> a :: b :: (c, pid_fmt x) :: tl in
    table#set_format fmt in
  if is_hex then on_change true;
  table, on_change

class t ~id (init : Errors.raw) control () =
  (* FIXME should remember preffered state *)
  let is_hex = false in
  let title = "Журнал ошибок" in
  let subtitle = "" in
  let title' = new Card.Primary.title ~large:true title () in
  let subtitle' = new Card.Primary.title ~large:true subtitle () in
  let text_box = Widget.create_div ~widgets:[title'; subtitle'] () in
  let table, on_change = make_table ~id is_hex init control in
  let media = new Card.Media.t ~widgets:[ table ] () in
  let switch = new Switch.t ~state:is_hex ~on_change () in
  let hex = new Form_field.t ~input:switch ~label:"HEX IDs" () in
  let primary = new Card.Primary.t ~widgets:[text_box; hex#widget] () in
  object(self)

    val mutable _has_more = true

    inherit Card.t ~widgets:[] ()

    method prepend_error (e : Errors.t) =
      let el = table#content in
      let top = el#scroll_top in
      let height = el#scroll_height in
      ignore @@ table#prepend_row (make_row_data e);
      let top' = el#scroll_top in
      if top <> 0 && top' = top
      then begin
          let diff = el#scroll_height - height in
          el#set_scroll_top (el#scroll_top + diff);
        end

    method append_error (e : Errors.t) =
      table#append_row (make_row_data e)

    initializer
      table#content#listen_lwt Widget.Event.scroll (fun _ _ ->
          let el = table#content in
          begin match _has_more,
                      el#client_height = el#scroll_height - el#scroll_top with
          | true, true ->
             let till =
               List.fold_left (fun acc row ->
                   let time =
                     let open Table in
                     match row#cells with
                     | t :: _ -> t#value in
                   match acc with
                   | None -> Some time
                   | Some acc ->
                      if Time.compare time acc >= 0
                      then Some acc else Some time) None table#rows in
             get_errors ?till ~limit:200 ~order:`Desc ~id control
             >|= (fun (more, l) -> _has_more <- more; List.rev l)
             >|= List.iter (ignore % self#append_error % snd)
             |> Lwt.map ignore
          | _ -> Lwt.return_unit
          end)
      |> Lwt.ignore_result;
      List.iter Fun.(ignore % self#prepend_error % snd) init;
      self#add_class base_class;
      self#append_child primary;
      self#append_child @@ new Divider.t ();
      self#append_child media;
  end

let make ?(init : (Errors.raw, string) Lwt_result.t option)
      (stream : Stream.t)
      (control : int) =
  let init = match init with
    | Some x -> x
    | None -> get_errors ~id:stream.id control
              >|= snd in
  init
  >|= (fun errors -> new t ~id:stream.id errors control ())
  |> Ui_templates.Loader.create_widget_loader
