open Containers
open Components
open Board_types
open Lwt_result.Infix

module Factory = struct

  (* Widget type *)
  type widget = Parameter of Widget_param.config
              | Chart of Widget_chart.config

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
    method create = function
      | Parameter conf ->
         let open Widget_param in
         let e = React.E.filter (fun (id,_) -> id = conf.id) self#get_measures in
         let on_destroy = fun () -> self#destroy_measures in
         (match conf.typ with
          | `Power   -> Power.make   ~on_destroy (React.E.map (fun (_,m) -> m.power) e)   conf
          | `Mer     -> Mer.make     ~on_destroy (React.E.map (fun (_,m) -> m.mer) e)     conf
          | `Ber     -> Ber.make     ~on_destroy (React.E.map (fun (_,m) -> m.ber) e)     conf
          | `Freq    -> Freq.make    ~on_destroy (React.E.map (fun (_,m) -> m.freq) e)    conf
          | `Bitrate -> Bitrate.make ~on_destroy (React.E.map (fun (_,m) -> m.bitrate) e) conf)
      | Chart conf ->
         let open Widget_chart in
         let event = self#get_measures in
         (match conf.typ with
          | `Power   -> Power.make ~init:[] ~event conf
          | `Mer     -> Mer.make   ~init:[] ~event conf
          | `Ber     -> Ber.make   ~init:[] ~event conf
          | `Freq    -> Freq.make  ~init:[] ~event conf
          | `Bitrate -> Freq.make  ~init:[] ~event conf)

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
