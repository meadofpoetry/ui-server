open Containers
open Components
open Lwt_result.Infix
open Api_js.Api_types
open Common

let name = "Errors"

let base_class = "qos-niit-errors-log"
let failure_class = Markup.CSS.add_modifier base_class "failure"
let dander_class = Markup.CSS.add_modifier base_class "danger"
let warning_class = Markup.CSS.add_modifier base_class "warning"

let ( >>* ) x f = Lwt_result.map_err f x
let ( % ) = Fun.( % )

let get_log ?from ?till ?duration ?limit ?order ?streams ?inputs () =
  let open Requests.HTTP in
  get_log ?limit ?from ?till ?duration ?streams ?inputs ()
  >>* Api_js.Requests.err_to_string
  >>= function
  | Raw s -> Lwt_result.return (s.has_more, s.data)
  | _ -> Lwt.fail_with "got compressed"

let pid_fmt hex =
  let open Table in
  let open Stream.Log_message in
  let to_string = fun (pid : pid) ->
    let f =
      if hex then Printf.sprintf "0x%04X"
      else Printf.sprintf "%04d" in
    let s = f pid.id in
    match pid.typ with
    | None -> s
    | Some typ -> Printf.sprintf "%s - %s" s typ in
  let compare = fun { id = a; _ } { id = b; _ } -> Int.compare a b in
  Custom { to_string; compare; is_numeric = false }

let make_row_data (item : Stream.Log_message.t) =
  let open Table in
  Data.(item.time :: item.message :: item.pid
        :: item.service :: item.info :: [])

let log_level_to_color level =
  let open Color in
  let open Stream.Log_message in
  let t = match level with
    | Info -> Color_palette.(make @@ Light_green C500)
    | Warn -> Color_palette.(make @@ Yellow C500)
    | Err -> Color_palette.(make @@ Orange C500)
    | Fatal -> Color_palette.(make @@ Red C900) in
  let Rgba.{ r; g; b; _ } = to_rgba t in
  Color.of_rgba r g b 0.5

class ['a] t ?inputs ?streams ?(init = []) () =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let show_time = Time.to_human_string ?tz_offset_s in
  let fmt =
    let sortable = false in
    let open Table in
    let open Format in
    (to_column ~sortable "Время", Time (Some show_time))
    :: (to_column ~sortable "Событие", String None)
    :: (to_column ~sortable "PID", Option (pid_fmt false, ""))
    :: (to_column ~sortable "Сервис", Option (String None, ""))
    :: (to_column "Подробности", String None)
    :: [] in
  object(self)

    val mutable _has_more = true

    inherit ['a] Table.t ~sticky_header:true
              ~dense:true ~fmt () as super

    method init () : unit =
      super#init ();
      (* FIXME implement infinite scroll *)
      self#content#listen_lwt Widget.Event.scroll (fun _ _ ->
          let el = self#content in
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
                      then Some acc else Some time) None self#rows in
             get_log ?till ~limit:200 ~order:`Desc ?streams ?inputs ()
             >|= (fun (more, l) -> _has_more <- more; List.rev l)
             >|= List.iter self#append_item
             |> Lwt.map ignore
          | _ -> Lwt.return_unit
          end)
      |> Lwt.ignore_result;
      List.iter self#prepend_item init;
      self#add_class base_class

    method prepend_item (e : Stream.Log_message.t) : unit =
      let el = self#content in
      let top = el#scroll_top in
      let height = el#scroll_height in
      let row = self#prepend_row (make_row_data e) in
      self#set_row_priority row e;
      let top' = el#scroll_top in
      if top <> 0 && top' = top
      then begin
          let diff = el#scroll_height - height in
          el#set_scroll_top (el#scroll_top + diff);
        end

    method append_item (e : Stream.Log_message.t) : unit =
      let row = self#append_row (make_row_data e) in
      self#set_row_priority row e

    (* Private methods *)

    method private set_row_priority (row : 'a Table.Row.t)
                     (i : Stream.Log_message.t) : unit =
      let el =
        let open Table in
        match row#cells with
        | _ :: cell :: _ -> cell in
      let bg_color = log_level_to_color i.level in
      let color = Color.text_color bg_color in
      el#style##.backgroundColor := (Js.string @@ Color.to_css_rgba bg_color);
      el#style##.color := (Js.string @@ Color.to_css_rgba color)

  end

let make ?inputs ?streams ?init () =
  new t ?init ?streams ?inputs ()

let make_dashboard_item ?settings ?init ?inputs ?streams ()
    : 'a Dashboard.Item.item =
  let w = make ?init ?inputs ?streams () in
  Dashboard.Item.make_item ~name:"Обзор" w
