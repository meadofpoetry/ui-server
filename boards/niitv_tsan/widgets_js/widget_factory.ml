open Application_types
open Board_niitv_tsan_http_js
open Containers
open Components

type item =
  | Settings
  | T2MI_settings [@@deriving yojson]

let item_to_info : item -> Dashboard.Item.info = fun item ->
  let serialized = item_to_yojson item in
  match item with
  | Settings ->
    Dashboard.Item.make_info ~title:Widget_settings.name
      ~thumbnail:(`Icon "settings")
      ~description:"Позволяет осуществлять настройку"
      ~serialized
      ()
  | T2MI_settings ->
    Dashboard.Item.make_info ~title:Widget_t2mi_settings.name
      ~thumbnail:(`Icon "settings")
      ~description:"Позволяет осуществлять настройку анализа потока T2-MI"
      ~serialized
      ()

let return = Lwt_result.return

let map_err x = Lwt_result.map_err Api_js.Http.error_to_string x

(* Widget factory *)
class t (control : int) () =
  let open Ui_templates.Factory in
  object(self)

    val mutable _state = None
    val mutable _t2mi_mode = None
    val mutable _jitter_mode = None
    val mutable _streams : Stream.t list Signal.t option = None
    val mutable _incoming_streams : Stream.t list Signal.t option = None

    (** Create widget of type *)
    method create : item -> 'a Dashboard.Item.item = function
      | Settings ->
        (fun state t2mi_mode streams ->
           Widget_settings.make ~state ~t2mi_mode ~streams control)
        |> Lift.l3 self#state self#t2mi_mode self#incoming_streams
        |> Ui_templates.Loader.create_widget_loader
        |> Dashboard.Item.make_item ~name:Widget_settings.name
      | T2MI_settings ->
        (fun state mode streams ->
           Widget_t2mi_settings.make ~state ~mode ~streams control )
        |> Lift.l3 self#state self#t2mi_mode self#incoming_streams
        |> Ui_templates.Loader.create_widget_loader
        |> Dashboard.Item.make_item ~name:Widget_t2mi_settings.name
          ?settings:Widget_t2mi_settings.settings

    method destroy () : unit =
      Option.iter State.finalize _state;
      Option.iter State.finalize _t2mi_mode;
      Option.iter State.finalize _jitter_mode;

    method available : Dashboard.available =
      `List [ item_to_info T2MI_settings
            ; item_to_info Settings ]

    method serialize (x : item) : Yojson.Safe.json =
      item_to_yojson x

    method deserialize (json : Yojson.Safe.json) : (item, string) result =
      item_of_yojson json

    (* Requests *)

    method state = match _state with
      | Some state -> state.value
      | None ->
        let state =
          Signal.make_state
            ~get:(fun () -> Http_device.get_state control |> map_err)
            ~get_socket:(fun f ->
                Http_device.Event.get_state f control) in
        _state <- Some state;
        state.value

    method t2mi_mode = match _t2mi_mode with
      | Some state -> state.value
      | None ->
        let state =
          Signal.make_state
            ~get:(fun () -> Http_device.get_t2mi_mode control |> map_err)
            ~get_socket:(fun f ->
                Http_device.Event.get_t2mi_mode f control) in
        _t2mi_mode <- Some state;
        state.value

    method incoming_streams = match _incoming_streams with
      | Some state -> state.value
      | None ->
        let state =
          Signal.make_state
            ~get:(fun () -> Http_streams.get_streams ~incoming:true control |> map_err)
            ~get_socket:(fun f ->
                Http_streams.Event.get_streams ~incoming:true f control) in
        _incoming_streams <- Some state;
        state.value

    method streams = match _streams with
      | Some state -> state.value
      | None ->
        let state =
          Signal.make_state
            ~get:(fun () -> Http_streams.get_streams control |> map_err)
            ~get_socket:(fun f -> Http_streams.Event.get_streams f control) in
        _streams <- Some state;
        state.value
  end
