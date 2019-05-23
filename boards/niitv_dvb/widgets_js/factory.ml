open Components
open Board_niitv_dvb_types
open Board_niitv_dvb_http_js
open Lwt_result.Infix

(* Widget type *)
type item =
  (* | Stream_chart of Widget_chart.widget_config option *)
  | Settings [@@deriving yojson]

let item_to_info : item -> Dashboard.Item.info = fun item ->
  let serialized = item_to_yojson item in
  match item with
  (* | Stream_chart _ ->
   *    Dashboard.Item.make_info
   *      ~title:"График"
   *      ~thumbnail:(`Icon "multiline_chart")
   *      ~description:"Отображает изменение выбранного измеряемого параметра во времени"
   *      ~serialized
   *      () *)
  | Settings ->
     Dashboard.Item.make_info
       ~title:"Настройки"
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку"
       ~serialized
       ()

let return = Lwt_result.return

let map_err x = Lwt_result.map_err Api_js.Http.error_to_string x

(* type measures = (id * Measure.t Time.timestamped) list *)

(* Widget factory *)
class t (control : int) () =
  let open Ui_templates.Factory in
  object(self)
    val mutable _state = None
    val mutable _config = None
    val mutable _receivers = None
    (* val mutable _measures = None *)

    (* val mutable _measures_ref = 0 *)

    (** Create widget of type *)
    method create : item -> Widget.t Dashboard.Item.item = function
      (* | Stream_chart conf -> self#_create_chart conf *)
      | Settings -> self#_create_settings ()

    method destroy () : unit =
      let iter f x = match x with None -> () | Some x -> f x in
      iter State.finalize _state;
      iter State.finalize _config;
      iter State.finalize _receivers;
      (* iter State.finalize _measures; *)

    method available : Dashboard.available =
      `List [ (* item_to_info (Stream_chart None)
             * ;  *)item_to_info Settings ]

    method serialize (x : item) : Yojson.Safe.json =
      item_to_yojson x

    method deserialize (json : Yojson.Safe.json) : (item, string) result =
      item_of_yojson json

    (** Private methods *)

    method private _create_settings () =
      Widget_settings.(
        (fun s c r -> make ~state:s ~config:c ~receivers:r control)
        |> Lift.l3 self#_state self#_config self#_receivers
        |> Ui_templates.Loader.create_widget_loader
        |> Widget.coerce
        |> Dashboard.Item.make_item ?settings ~name)

    (* method private _create_chart conf =
     *   (\* FIXME conf should not be an option *\)
     *   let conf = Option.get_exn conf in
     *   let init =
     *     Requests.History.HTTP.Measurements.get
     *       ~ids:conf.sources
     *       ~duration:(conf.duration)
     *       control
     *     |> Lwt_result.map (function Api_js.Api_types.Raw x -> x.data
     *                               | _ -> assert false)
     *     |> Lwt_result.map_err Api_js.Requests.err_to_string in
     *   init
     *   >|= (fun init -> Widget_chart.make ~init ~measures:self#_measures conf)
     *   |> Ui_templates.Loader.create_widget_loader
     *   |> Widget.coerce
     *   |> Dashboard.Item.make_item
     *        ~name:(Widget_types.measure_type_to_string conf.typ) *)

    method private _state = match _state with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Http_device.get_state control |> map_err)
             ~get_socket:(fun f -> Http_device.Event.get_state f control) in
         _state <- Some state;
         state.value

    method private _config = match _config with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Http_device.get_mode control |> map_err)
             ~get_socket:(fun f -> Http_device.Event.get_mode f control) in
         _config <- Some state;
         state.value

    method private _receivers = match _receivers with
      | Some state -> state.value
      | None ->
         let state =
           Signal.make_state
             ~get:(fun () -> Http_device.get_receivers control |> map_err)
             ~get_socket:(fun f -> Http_device.Event.get_receivers f control) in
         _receivers <- Some state;
         state.value

    (* method private _measures : measures React.event = match _measures with
     *   | Some state -> state.value
     *   | None ->
     *      let e, sock = Requests.Streams.WS.get_measures control in
     *      let fin = fun () -> sock##close; React.E.stop ~strong:true e in
     *      let state = State.make ~fin e in
     *      _measures <- Some state;
     *      e *)

  end
