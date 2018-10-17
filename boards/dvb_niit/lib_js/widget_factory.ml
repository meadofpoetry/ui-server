open Containers
open Components
open Common
open Board_types
open Lwt_result.Infix
open Ui_templates.Factory

(* Widget type *)
type item =
  | Module_measure  of Widget_module_measure.config option
  | Module_measures of Widget_module_measures.config option
  | Measures        of Widget_measures.config option
  | Measure         of Widget_measure.config option
  | Chart           of Widget_chart.config option
  | Module_settings of Widget_module_settings.config option
  | Settings        of Widget_settings.config option [@@deriving yojson]

let item_to_info : item -> Dashboard.Item.info = fun item ->
  let serialized = item_to_yojson item in
  match item with
  | Module_measure _  ->
     Dashboard.Item.to_info
       ~title:"Параметр модуля"
       ~thumbnail:(`Icon "show_chart")
       ~description:"Отображает выбранный измеряемый параметр для выбранного модуля"
       ~serialized
       ()
  | Module_measures _ ->
     Dashboard.Item.to_info
       ~title:"Параметры модуля"
       ~thumbnail:(`Icon "show_chart")
       ~description:"Отображает все измеряемые параметры для выбранного модуля"
       ~serialized
       ()
  | Measures _        ->
     Dashboard.Item.to_info
       ~title:"Параметры"
       ~thumbnail:(`Icon "show_chart")
       ~description:"Отображает все измеряемые параметры"
       ~serialized
       ()
  | Measure _         ->
     Dashboard.Item.to_info
       ~title:"Параметр"
       ~thumbnail:(`Icon "show_chart")
       ~description:"Отображает выбранный измеряемый параметр"
       ~serialized
       ()
  | Chart _           ->
     Dashboard.Item.to_info
       ~title:"График"
       ~thumbnail:(`Icon "multiline_chart")
       ~description:"Отображает изменение выбранного измеряемого параметра во времени"
       ~serialized
       ()
  | Module_settings _ ->
     Dashboard.Item.to_info
       ~title:"Настройки модуля"
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку выбранного модуля" ()
       ~serialized
  | Settings _        ->
     Dashboard.Item.to_info
       ~title:"Настройки"
       ~thumbnail:(`Icon "settings")
       ~description:"Позволяет осуществлять настройку"
       ~serialized
       ()

let return = Lwt_result.return

let map_err : 'a 'b. ('b,'a Api_js.Requests.err) Lwt_result.t -> ('b,string) Lwt_result.t =
  fun x -> Lwt_result.map_err (fun e -> Api_js.Requests.err_to_string ?to_string:None e) x

open Factory_state

(* Widget factory *)
class t (control:int) () =
object(self)
  val mutable _state    : Topology.state React.signal t_lwt    = empty ()
  val mutable _config   : config React.signal t_lwt            = empty ()
  val mutable _measures : (int * measures) React.event Factory_state.t = empty ()

  val mutable _measures_ref = 0

  (** Create widget of type **)
  method create : item -> Dashboard.Item.item = function
    | Module_measure conf  -> self#_create_module_measure conf
    | Module_measures conf -> self#_create_module_measures conf
    | Measures conf        -> self#_create_measures conf
    | Measure conf         -> self#_create_measure conf
    | Chart conf           -> self#_create_chart conf
    | Module_settings conf -> self#_create_module_settings conf
    | Settings conf        -> self#_create_settings conf

  method destroy () =
    finalize _state;
    finalize _config;
    finalize _measures

  method available : Dashboard.available =
    `List [ item_to_info (Measures None)
          ; item_to_info (Measure None)
          ; item_to_info (Chart None)
          ; item_to_info (Settings None)
          ; item_to_info (Module_measure None)
          ; item_to_info (Module_measures None)
          ; item_to_info (Module_settings None)
          ]

  method serialize (x : item) : Yojson.Safe.json = item_to_yojson x
  method deserialize (json : Yojson.Safe.json) : (item,string) result = item_of_yojson json

  (** Private methods **)

  method private _create_module_measure conf =
    Widget_module_measure.make ~measures:self#_measures conf
    |> Dashboard.Item.to_item ~name:(Widget_module_measure.name conf)
         ?settings:Widget_module_measure.settings

  method private _create_module_measures conf =
    Widget_module_measures.make ~measures:self#_measures conf
    |> Dashboard.Item.to_item ~name:(Widget_module_measures.name conf)
         ?settings:Widget_module_measures.settings

  method private _create_measures conf =
    (fun c -> Widget_measures.make ~measures:self#_measures ~config:c conf)
    |> Factory_state_lwt.l1 self#_config
    |> Ui_templates.Loader.create_widget_loader
    |> Dashboard.Item.to_item ~name:Widget_measures.name
         ?settings:Widget_measures.settings

  method private _create_measure conf =
    (fun c -> Widget_measure.make ~measures:self#_measures ~config:c conf)
    |> Factory_state_lwt.l1 self#_config
    |> Ui_templates.Loader.create_widget_loader
    |> Dashboard.Item.to_item ~name:(Widget_measure.name conf)
         ?settings:Widget_measure.settings

  method private _create_module_settings conf =
    (fun s c -> Widget_module_settings.make ~state:s ~config:c conf control)
    |> Factory_state_lwt.l2 self#_state self#_config
    |> Ui_templates.Loader.create_widget_loader
    |> Dashboard.Item.to_item ~name:(Widget_module_settings.name conf)
         ?settings:Widget_module_settings.settings

  method private _create_settings conf =
    (fun s c -> Widget_settings.make ~state:s ~config:c conf control)
    |> Factory_state_lwt.l2 self#_state self#_config
    |> Ui_templates.Loader.create_widget_loader
    |> Dashboard.Item.to_item ~name:Widget_settings.name
         ?settings:Widget_settings.settings

  method private _create_chart conf =
    Widget_chart.make ~measures:self#_measures conf

  method private _state =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Device.HTTP.get_state control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_state control)
      _state

  method private _config =
    Factory_state_lwt.get_value_as_signal
      ~get:(fun () -> Requests.Device.HTTP.get_config control |> map_err)
      ~get_socket:(fun () -> Requests.Device.WS.get_config control)
      _config

  method private _measures = match _measures.value with
    | Some x -> Factory_state.succ_ref _measures; x
    | None   -> Factory_state.set_ref _measures 1;
                let e,sock = Requests.Receivers.WS.get_measures control in
                _measures.value <- Some e;
                _measures.fin   <- (fun () -> sock##close; React.E.stop ~strong:true e);
                e

end
