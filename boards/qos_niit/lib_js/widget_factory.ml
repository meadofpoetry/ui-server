open Containers
open Components
open Board_types
open Lwt_result.Infix
open Ui_templates.Factory
open Common.Topology

type item =
  | Structure       of Widget_structure.config option
  | Settings        of Widget_settings.config option
  | T2MI_settings   of Widget_t2mi_settings.config option
  | Jitter_settings of Widget_jitter_settings.config option [@@deriving yojson]

let item_to_info : item -> Dashboard.Item.info = fun item ->
  let serialized = item_to_yojson item in
  match item with
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

  val _state    : state React.signal t_lwt                 = empty ()
  val _config   : config React.signal t_lwt                = empty ()
  val _structs  : Streams.TS.structures React.signal t_lwt = empty ()
  val _bitrates : Streams.TS.structures React.signal t_lwt = empty ()

  (** Create widget of type **)
  method create : item -> Dashboard.Item.item = function
    | Structure conf       -> (fun s str -> Widget_structure.make ~state:s ~structs:str conf)
                              |> Factory_state_lwt.l2 self#_state self#_structs
                              |> Ui_templates.Loader.create_widget_loader
                              |> Dashboard.Item.to_item ~name:Widget_structure.name
                                                        ?settings:Widget_structure.settings
    | Settings conf        -> (fun s c -> Widget_settings.make ~state:s ~config:c
                                                               ~streams:self#_streams
                                                               conf control)
                              |> Factory_state_lwt.l2 self#_state self#_config
                              |> Ui_templates.Loader.create_widget_loader
                              |> Dashboard.Item.to_item ~name:Widget_settings.name
                                                        ?settings:Widget_settings.settings
    | T2MI_settings conf   -> (fun s c -> Widget_t2mi_settings.make ~state:s ~config:c
                                                                    ~streams:self#_streams
                                                                    conf control )
                              |> Factory_state_lwt.l2 self#_state self#_config
                              |> Ui_templates.Loader.create_widget_loader
                              |> Dashboard.Item.to_item ~name:Widget_t2mi_settings.name
                                                        ?settings:Widget_t2mi_settings.settings
    | Jitter_settings conf -> (fun s c -> Widget_jitter_settings.make ~state:s ~config:c conf control)
                              |> Factory_state_lwt.l2 self#_state self#_config
                              |> Ui_templates.Loader.create_widget_loader
                              |> Dashboard.Item.to_item ~name:Widget_jitter_settings.name
                                                        ?settings:Widget_jitter_settings.settings

  method destroy () : unit = Factory_state.finalize _state;
                             Factory_state.finalize _config;
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
      ~get:(fun () -> Requests.get_state control |> map_err)
      ~get_socket:(fun () -> Requests.get_state_ws control)
      _state

  method private _structs =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Streams.Real_time.get_ts_structs control |> map_err)
      ~get_socket:(fun () -> Requests.Streams.Real_time.get_ts_structs_ws control)
      _structs

  method private _config =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Board.Real_time.get_config control |> map_err)
      ~get_socket:(fun () -> Requests.Board.Real_time.get_config_ws control)
      _config

  method private _bitrates =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Streams.Real_time.get_ts_bitrates control |> map_err)
      ~get_socket:(fun () -> Requests.Streams.Real_time.get_ts_bitrates_ws control)
      _bitrates

  method private _streams = React.S.const []

end
