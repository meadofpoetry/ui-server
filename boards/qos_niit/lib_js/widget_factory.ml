open Containers
open Components
open Board_types
open Lwt_result.Infix
open Ui_templates.Factory
open Common.Topology
open Common

type item =
  | Chart           of Widget_chart.config option
  | Structure       of Widget_structure.config option
  | Settings        of Widget_settings.config option
  | T2MI_settings   of Widget_t2mi_settings.config option
  | Jitter_settings of Widget_jitter_settings.config option [@@deriving yojson]

let item_to_info : item -> Dashboard.Item.info = fun item ->
  let serialized = item_to_yojson item in
  match item with
  | Chart _ ->
     Dashboard.Item.to_info ~title:"График"
       ~thumbnail:(`Icon "chart")
       ~description:"График"
       ~serialized
       ()
  | Structure _ ->
     Dashboard.Item.to_info ~title:"Структура потока"
       ~thumbnail:(`Icon "list")
       ~description:"Отображает структуру обнаруженных транспортных потоков"
       ~serialized
       ()
  | Settings _ ->
     Dashboard.Item.to_info ~title:Widget_settings.name
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку"
       ~serialized
       ()
  | T2MI_settings _ ->
     Dashboard.Item.to_info ~title:Widget_t2mi_settings.name
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку анализа потока T2-MI"
       ~serialized
       ()
  | Jitter_settings _ ->
     Dashboard.Item.to_info ~title:Widget_jitter_settings.name
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку измерений джиттера"
       ~serialized
       ()

let return = Lwt_result.return

let map_err : 'a 'b. ('b,'a Api_js.Requests.err) Lwt_result.t -> ('b,string) Lwt_result.t =
  fun x -> Lwt_result.map_err (fun e -> Api_js.Requests.err_to_string ?to_string:None e) x

open Factory_state

(* Widget factory *)
class t (control:int) () =
object(self)

  val _state       : state React.signal t_lwt              = empty ()
  val _t2mi_mode   : t2mi_mode option   React.signal t_lwt = empty ()
  val _jitter_mode : jitter_mode option React.signal t_lwt = empty ()
  val _structs     : (Stream.id * Streams.TS.structure) list React.signal t_lwt = empty ()
  val _bitrates    : (Stream.id * Streams.TS.bitrate) list React.signal t_lwt = empty ()

  (** Create widget of type **)
  method create : item -> Dashboard.Item.item = function
    | Chart conf ->
       Widget_chart.make conf
       |> Dashboard.Item.to_item ~name:Widget_chart.name
    | Structure config ->
       (fun s str -> Widget_structure.make ?config ~state:s ~signal:str control ())
       |> Factory_state_lwt.l2 self#_state self#_structs
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_structure.name
            ?settings:Widget_structure.settings
    | Settings conf ->
       (fun s t j -> Widget_settings.make ~state:s ~t2mi_mode:t ~jitter_mode:j
                       ~streams:self#_streams
                       conf control)
       |> Factory_state_lwt.l3 self#_state self#_t2mi_mode self#_jitter_mode
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_settings.name
            ?settings:Widget_settings.settings
    | T2MI_settings conf ->
       (fun s m -> Widget_t2mi_settings.make ~state:s ~mode:m
                     ~streams:self#_streams
                     conf control )
       |> Factory_state_lwt.l2 self#_state self#_t2mi_mode
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_t2mi_settings.name
            ?settings:Widget_t2mi_settings.settings
    | Jitter_settings conf ->
       (fun s m -> Widget_jitter_settings.make ~state:s ~mode:m conf control)
       |> Factory_state_lwt.l2 self#_state self#_jitter_mode
       |> Ui_templates.Loader.create_widget_loader
       |> Dashboard.Item.to_item ~name:Widget_jitter_settings.name
            ?settings:Widget_jitter_settings.settings

  method destroy () : unit = Factory_state.finalize _state;
                             Factory_state.finalize _t2mi_mode;
                             Factory_state.finalize _jitter_mode;
                             Factory_state.finalize _structs;
                             Factory_state.finalize _bitrates

  method available : Dashboard.available =
    `List [ item_to_info (Structure None)
          ; item_to_info (T2MI_settings None)
          ; item_to_info (Jitter_settings None)
          ; item_to_info (Settings None)
          ]

  method serialize (x : item) : Yojson.Safe.json = item_to_yojson x
  method deserialize (json : Yojson.Safe.json) : (item,string) result = item_of_yojson json

  (** Private methods **)

  method private _state =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun ()        -> Requests.Device.HTTP.get_state control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_state control)
      _state

  method private _t2mi_mode =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun ()        -> Requests.Device.HTTP.get_t2mi_mode control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_t2mi_mode control)
      _t2mi_mode

  method private _jitter_mode =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () ->        Requests.Device.HTTP.get_jitter_mode control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_jitter_mode control)
      _jitter_mode

  method private _structs =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun ()        -> Requests.Streams.HTTP.TS.get_structure control |> map_err)
      ~get_socket:(fun () -> Requests.Streams.WS.TS.get_structure control)
      _structs

  method private _bitrates =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun ()        -> Requests.Streams.HTTP.TS.get_bitrate control |> map_err)
      ~get_socket:(fun () -> Requests.Streams.WS.TS.get_bitrate control)
      _bitrates

  method private _streams = React.S.const []

end
