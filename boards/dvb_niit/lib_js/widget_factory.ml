open Containers
open Components
open Board_types
open Lwt_result.Infix

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
     Dashboard.Item.to_info ~title:"Параметр (для модуля)"
                            ~thumbnail:(`Icon "show_chart")
                            ~description:"Отображает выбранный измеряемый параметр для выбранного модуля"
                            ~serialized
                            ()
  | Module_measures _ ->
     Dashboard.Item.to_info ~title:"Параметры (для модуля)"
                            ~thumbnail:(`Icon "show_chart")
                            ~description:"Отображает все измеряемые параметры для выбранного модуля"
                            ~serialized
                            ()
  | Measures _        ->
     Dashboard.Item.to_info ~title:"Параметры"
                            ~thumbnail:(`Icon "show_chart")
                            ~description:"Отображает все измеряемые параметры"
                            ~serialized
                            ()
  | Measure _         ->
     Dashboard.Item.to_info ~title:"Параметр"
                            ~thumbnail:(`Icon "show_chart")
                            ~description:"Отображает выбранный измеряемый параметр"
                            ~serialized
                            ()
  | Chart _           ->
     Dashboard.Item.to_info ~title:"График"
                            ~thumbnail:(`Icon "multiline_chart")
                            ~description:"Отображает изменение выбранного измеряемого параметра во времени"
                            ~serialized
                            ()
  | Module_settings _ ->
     Dashboard.Item.to_info ~title:"Настройки (для модуля)"
                            ~thumbnail:(`Icon "settings")
                            ~description:"Позволяет осуществлять настройку выбранного модуля" ()
                            ~serialized
  | Settings _        ->
     Dashboard.Item.to_info ~title:"Настройки"
                            ~thumbnail:(`Icon "settings")
                            ~description:"Позволяет осуществлять настройку"
                            ~serialized
                            ()

let return = Lwt_result.return

(* Widget factory *)
class t (control:int) ()  =
object(self)
  val mutable _state    = None
  val mutable _config   = None
  val mutable _measures = None

  val mutable _state_ref    = 0
  val mutable _config_ref   = 0
  val mutable _measures_ref = 0

  (** Create widget of type **)
  method create : item -> Dashboard.Item.item = function
    | Module_measure conf  -> Widget_module_measure.make ~measures:self#get_measures conf
    | Module_measures conf -> Widget_module_measures.make ~measures:self#get_measures conf
    | Measures conf        -> Widget_measures.make ~measures:self#get_measures
                                                   ~config:self#get_config
                                                   conf
    | Measure conf         -> Widget_measure.make ~measures:self#get_measures
                                                  ~config:self#get_config
                                                  conf
    | Chart conf           -> Widget_chart.make ~measures:self#get_measures conf
    | Module_settings conf -> Widget_module_settings.make ~state:self#get_state
                                                          ~config:self#get_config
                                                          conf control
    | Settings conf        -> Widget_settings.make ~state:self#get_state
                                                   ~config:self#get_config
                                                   conf control

  method destroy () = _state_ref <- 0; _config_ref <- 0; _measures_ref <- 0;
                      self#destroy_state; self#destroy_config; self#destroy_measures

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

  method private destroy_state =
    _state_ref <- _state_ref - 1;
    if _state_ref <= 0
    then (Option.iter (fun t -> t >>= (fun (_,f) -> return @@ f ()) |> Lwt.ignore_result) _state;
          _state <- None)
  method private destroy_config =
    _config_ref <- _config_ref - 1;
    if _config_ref <= 0
    then (Option.iter (fun t -> t >>= (fun (_,f) -> return @@ f ()) |> Lwt.ignore_result) _config;
          _config <- None)
  method private destroy_measures =
    _measures_ref <- _measures_ref - 1;
    if _measures_ref <= 0
    then (Option.iter (fun (_,f) -> f ()) _measures;
          _measures <- None)

  method private get_state = match _state with
    | Some x -> _state_ref <- _state_ref + 1; Lwt_result.map fst x
    | None   ->
       _state_ref <- 1;
       let t = Requests.get_state control
               >>= (fun state -> let e,sock = Requests.get_state_ws control in
                                 let s      = React.S.hold state e in
                                 let fin () = sock##close;
                                              React.E.stop ~strong:true e;
                                              React.S.stop ~strong:true s
                                 in
                                 return (s,fin))
               |> Lwt.map (function Ok x    -> Ok x
                                  | Error e -> Error (Api_js.Requests.err_to_string e))
       in
       _state <- Some t;
       Lwt_result.map fst t

  method private get_config = match _config with
    | Some x -> _config_ref <- _config_ref + 1; Lwt_result.map fst x
    | None   ->
       _config_ref <- 1;
       let t = Requests.get_config control
               >>= (fun config -> let e,sock = Requests.get_config_ws control in
                                  let s      = React.S.hold config e in
                                  let fin () = sock##close;
                                               React.E.stop ~strong:true e;
                                               React.S.stop ~strong:true s
                                  in
                                  return (s,fin))
               |> Lwt.map (function Ok x    -> Ok x
                                  | Error e -> Error (Api_js.Requests.err_to_string e))
       in
       _config <- Some t;
       Lwt_result.map fst t

  method private get_measures = match _measures with
    | Some x -> _measures_ref <- _measures_ref + 1; fst x
    | None   ->
       _measures_ref <- 1;
       let e,sock = Requests.get_measures_ws control in
       let fin () = sock##close; React.E.stop ~strong:true e in
       _measures <- Some (e,fin);
       e

end
