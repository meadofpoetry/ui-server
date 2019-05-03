open Containers
open Components
open Pipeline_types
open Util_react
open Ui_templates.Factory
open Lwt_result.Infix
open Pipeline_api_js

let ( % ) f g x = f (g x)

let ( >>= ) = Lwt.( >>= )

type item =
  | Chart of Widget_parameter_chart.widget_config option [@@deriving yojson]

class t () =
object(self)

  val mutable _structures = None
  val mutable _audio_data = None
  val mutable _video_data :
    Qoe_errors.Video_data.t list event State.t option = None

  method create : item -> Widget.t Dashboard.Item.item = function
    | Chart cfg ->
      let open Widget_parameter_chart in
      let config = Option.get_exn cfg in
      let structures = self#get_structures () in
      let t =
        structures
        >>= function
        | Error e -> Lwt.return_error e
        | Ok structures ->
        let chart = new t ~init:[] ~structures ~config () in
        match typ_to_content config.typ with
        | `Video ->
          (self#get_video_data ()
           >>= function
           | Error e -> Lwt.return_error e
           | Ok event ->
             E.map (fun (data : Qoe_errors.Video_data.t list) ->
                 let data = convert_video_data config data in
                 chart#append_data data) event
             |> E.keep;
             Lwt.return_ok chart)
        | `Audio ->
          (self#get_audio_data ()
           >>= function
           | Error e -> Lwt.return_error e
           | Ok event ->
             E.map (fun (data : Qoe_errors.Audio_data.t list) ->
                 let data = convert_audio_data config data in
                 chart#append_data data) event
             |> E.keep;
             Lwt.return_ok chart) in
      let w = Ui_templates.Loader.create_widget_loader t in
      Dashboard.Item.make_item
        ~name:(typ_to_string config.typ)
        w#widget

  method destroy () : unit =
    Option.iter State.finalize _structures;
    Option.iter State.finalize _audio_data;
    Option.iter State.finalize _video_data;

  method available : Dashboard.available =
    `List []

  method serialize (x : item) : Yojson.Safe.json =
    item_to_yojson x

  method deserialize (json : Yojson.Safe.json)
         : (item, string) result =
    item_of_yojson json

  (* Private methods *)

  method private get_structures () = match _structures with
    | Some (state : _ State.t) -> state.value
    | None ->
      let state =
        let get () =
          Api_structure.get_streams_with_source ~applied:true ()
          >>= function
          | Ok x -> Lwt.return_ok x
          | Error e -> Lwt.return_error @@ Api_js.Http.error_to_string e in
        let get_socket ~f () =
          Api_structure.Event.get_streams_with_source ~applied:true ~f () in
        Signal.make_state
          ~get
          ~get_socket in
      _structures <- Some state;
      state.value

  (* XXX
   * Some thoughts about optimization:
   * Maybe this function should make request according to current widget needs
   * and when these needs change it should close an old socket and open a new one
   * with new filtering applied.
   * E.g., Requesting a socket with data for all sources is very redundant for one widget
   *)
  method private get_video_data () = match _video_data with
    | Some state -> Lwt.return_ok state.value
    | None ->
      let e, set = React.E.create () in
      Api_measurements.Event.get_video ~f:(fun _ -> function
          | Error _ -> ()
          | Ok x -> set x) ()
      >>= function
      | Error e -> Lwt.return_error e
      | Ok socket ->
      let fin = fun () ->
        Api_js.Websocket.close socket;
        React.E.stop ~strong:true e in
      let state = State.make ~fin e in
      _video_data <- Some state;
      Lwt.return_ok e

  method private get_audio_data () = match _audio_data with
    | Some state -> Lwt.return_ok state.value
    | None ->
      let e, set = React.E.create () in
      Api_measurements.Event.get_audio ~f:(fun _ -> function
          | Error _ -> ()
          | Ok x -> set x) ()
      >>= function
      | Error e -> Lwt.return_error e
      | Ok socket ->
      let fin = fun () ->
        Api_js.Websocket.close socket;
        React.E.stop ~strong:true e in
      let state = State.make ~fin e in
      _audio_data <- Some state;
      Lwt.return_ok e

end
