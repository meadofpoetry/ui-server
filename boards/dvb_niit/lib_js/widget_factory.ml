open Containers
open Components
open Common
open Board_types
open Lwt_result.Infix
open Ui_templates.Factory

(* Widget type *)
type item =
  | Stream_chart of Widget_chart.config option
  | Settings [@@deriving yojson]

let item_to_info : item -> Dashboard.Item.info = fun item ->
  let serialized = item_to_yojson item in
  match item with
  | Stream_chart _ ->
     Dashboard.Item.make_info
       ~title:"График"
       ~thumbnail:(`Icon "multiline_chart")
       ~description:"Отображает изменение выбранного измеряемого параметра во времени"
       ~serialized
       ()
  | Settings ->
     Dashboard.Item.make_info
       ~title:"Настройки"
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку"
       ~serialized
       ()

let return = Lwt_result.return

let map_err : 'a 'b. ('b,'a Api_js.Requests.err) Lwt_result.t -> ('b,string) Lwt_result.t =
  fun x -> Lwt_result.map_err (fun e -> Api_js.Requests.err_to_string ?to_string:None e) x

open Factory_state

type measures = Stream.t * (Measure.t Time.timestamped)

(* Widget factory *)
class t (control:int) () =
object(self)
  val mutable _state : Topology.state React.signal t_lwt = empty ()
  val mutable _config : Device.config React.signal t_lwt = empty ()
  val mutable _receivers : int list option React.signal t_lwt = empty ()
  val mutable _measures : measures React.event Factory_state.t = empty ()

  val mutable _measures_ref = 0

  (** Create widget of type **)
  method create : item -> Widget.t Dashboard.Item.item = function
    | Stream_chart conf -> self#_create_chart conf
    | Settings -> self#_create_settings ()

  method destroy () =
    finalize _state;
    finalize _config;
    finalize _measures

  method available : Dashboard.available =
    `List [ item_to_info (Stream_chart None)
          ; item_to_info Settings
          ]

  method serialize (x : item) : Yojson.Safe.json = item_to_yojson x
  method deserialize (json : Yojson.Safe.json) : (item, string) result = item_of_yojson json

  (** Private methods **)

  method private _create_settings () =
    (fun s c r ->
      Widget_settings.make ~state:s ~config:c ~receivers:r control)
    |> Factory_state_lwt.l3 self#_state self#_config self#_receivers
    |> Ui_templates.Loader.create_widget_loader
    |> Widget.coerce
    |> Dashboard.Item.make_item ~name:Widget_settings.name
         ?settings:Widget_settings.settings

  method private _create_chart conf =
    Widget_chart.make ~measures:self#_measures conf
    |> fun x -> { x with widget = x.widget#widget }

  method private _state =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Device.HTTP.get_state control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_state control)
      _state

  method private _config =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Device.HTTP.get_mode control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_mode control)
      _config

  method private _receivers =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Device.HTTP.get_receivers control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_receivers control)
      _receivers

  method private _measures : measures React.event =
    match _measures.value with
    | Some x ->
       Factory_state.succ_ref _measures; x
    | None ->
       Factory_state.set_ref _measures 1;
       let e,sock = Requests.Streams.WS.get_measures control in
       _measures.value <- Some e;
       _measures.fin   <- (fun () -> sock##close; React.E.stop ~strong:true e);
       e

end
