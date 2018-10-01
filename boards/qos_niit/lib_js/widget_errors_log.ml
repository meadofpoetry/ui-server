open Containers
open Components
open Common
open Board_types
open Board_types
open Lwt_result.Infix
open Api_js.Api_types

type config =
  { stream : Stream.t
  }

type state_list = state list [@@deriving show]

let name = "Errors"

let base_class = "qos-niit-errors-log"
let failure_class = Markup.CSS.add_modifier base_class "failure"
let dander_class = Markup.CSS.add_modifier base_class "danger"
let warning_class = Markup.CSS.add_modifier base_class "warning"

let settings = None

let ( >>* ) x f = Lwt_result.map_err f x
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

      (* Applies changes to the signal *)
      method apply () : unit =
        let hex = hex_switch#checked in
        set { hex }

      (* Resets UI controls to current signal state *)
      method reset () : unit =
        let { hex } = React.S.value self#s in
        hex_switch#set_checked hex

      method s : t React.signal = s

    end

  let make () = new view ()

end

let get_errors ?from ?till ?duration ?limit ?order ~id control =
  let open Requests.History.HTTP.Errors in
  get ?limit ?from ?till ?duration ?order ~ids:[id] control
  >>* Api_js.Requests.err_to_string
  >>= function
  | Raw s -> Lwt_result.return (s.has_more, s.data)
  | _ -> Lwt.fail_with "got compressed"

let get_state ?from ?till ?duration ?limit control =
  let open Requests.Device.HTTP.Archive in
  get_state ?limit ?from ?till ?duration control
  >>* Api_js.Requests.err_to_string
  >>= function
  | Raw s -> Lwt_result.return (s.data : state list)
  | _ -> Lwt.fail_with "got compressed"

let get_stream ?from ?till ?duration ?limit ~id control =
  let open Requests.History.HTTP.Streams in
  get ?limit ?from ?till ?duration ~ids:[id] control
  >>* Api_js.Requests.err_to_string

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

let make_row_data ({ data = error; timestamp } : Error.t timestamped) =
  let open Table in
  let service = error.service_name in
  let pid = error.pid, error.multi_pid in
  let count = error.count in
  let check =
    let num, name = Ts_error.to_name error in
    num ^ " " ^ name in
  let extra = Ts_error.Description.of_ts_error error in
  Data.(timestamp :: check :: pid :: service :: count :: extra :: [])

class ['a] t ~id (init : Error.raw) control () =
  let settings = Settings.make () in
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
  object(self)

    val mutable _has_more = true

    inherit ['a] Table.t ~sticky_header:true
              ~dense:true ~fmt ()

    method settings_widget : Settings.view =
      settings

    method set_settings (x : Settings.t) : unit =
      let ({ hex } : Settings.t) = x in
      self#set_hex hex

    method set_hex (x : bool) : unit =
      let fmt =
        let open Table.Format in
        match fmt with
        | a :: b :: (c, _) :: tl -> a :: b :: (c, pid_fmt x) :: tl in
      self#set_format fmt

    method prepend_error (e : Error.t timestamped) : unit =
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

    method append_error (e : Error.t timestamped) : unit =
      let row = self#append_row (make_row_data e) in
      self#set_row_priority row e

    (* Private methods *)

    method private set_row_priority (row : 'a Table.Row.t)
                     (e : Error.t timestamped) =
      let el =
        let open Table in
        match row#cells with
        | _ :: cell :: _ -> cell in
      match e.data.priority with
      | 1 -> el#add_class failure_class
      | 2 -> el#add_class dander_class
      | 3 -> el#add_class warning_class
      | _ -> ()

    initializer
      React.S.map self#set_settings settings#s
      |> self#_keep_s;
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
             get_errors ?till ~limit:200 ~order:`Desc ~id control
             >|= (fun (more, l) -> _has_more <- more; List.rev l)
             >|= List.iter (self#append_error % snd)
             |> Lwt.map ignore
          | _ -> Lwt.return_unit
          end)
      |> Lwt.ignore_result;
      List.iter (self#prepend_error % snd) init;
      self#add_class base_class;
  end

let make ?(init : (Error.raw, string) Lwt_result.t option)
      (stream : Stream.t)
      (control : int) =
  let state = get_state control in
  state
  |> Lwt.map (function Error e -> print_endline e; Error e | Ok x -> Ok x)
  >|= (fun x -> print_endline @@ show_state_list x)
  |> Lwt.ignore_result;
  let init = match init with
    | Some x -> x
    | None -> get_errors ~id:stream.id control
              >|= snd in
  init
  >|= (fun errors -> new t ~id:stream.id errors control ())
  |> Ui_templates.Loader.create_widget_loader
