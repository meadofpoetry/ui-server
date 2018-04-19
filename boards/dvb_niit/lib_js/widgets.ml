open Containers
open Components
open Board_types
open Lwt_result.Infix

module Factory = struct

  (* Widget type *)
  type widget = Parameter       of Widget_param.config
              | Parameters      of Widget_params.config
              | Chart           of Widget_chart.config
              | Module_settings of Widget_module_settings.config
              | Settings

  let return = Lwt_result.return

  (* Widget factory *)
  class t (control:int) () =
  object(self)
    val mutable _state    = None
    val mutable _config   = None
    val mutable _measures = None

    val mutable _state_ref    = 0
    val mutable _config_ref   = 0
    val mutable _measures_ref = 0

    (** Create widget of type **)
    method create : widget -> unit Widget_grid.Item.t = function
      | Parameter conf       -> Widget_param.make ~measures:self#get_measures conf
      | Parameters conf      -> Widget_params.make ~measures:self#get_measures conf
      | Chart conf           -> Widget_chart.make ~measures:self#get_measures conf
      | Module_settings conf -> Widget_module_settings.make ~state:self#get_state
                                                            ~config:self#get_config
                                                            conf control
      | Settings             -> Widget_settings.make ~state:self#get_state
                                                     ~config:self#get_config
                                                     control

    method destroy = _state_ref <- 0; _config_ref <- 0; _measures_ref <- 0;
                     self#destroy_state; self#destroy_config; self#destroy_measures

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

end
