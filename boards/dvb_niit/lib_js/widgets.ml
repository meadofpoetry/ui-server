open Containers
open Components
open Board_types
open Lwt_result.Infix

module type Measure_params = sig
  type t
  val name : string
  val unit : string
  val data_to_string : t -> string
end

module Meas = struct
  module Power   : Measure_params with type t = float = struct
    type t = float
    let name = "Мощность"
    let unit = "дБм"
    let data_to_string = Printf.sprintf "%.2f"
  end
  module Mer     : Measure_params with type t = float = struct
    type t   = float
    let name = "MER"
    let unit = "дБ"
    let data_to_string = Printf.sprintf "%.2f"
  end
  module Ber     : Measure_params with type t = float = struct
    type t   = float
    let name = "BER"
    let unit = ""
    let data_to_string = Printf.sprintf "%.2f"
  end
  module Freq    : Measure_params with type t = int32 = struct
    type t   = int32
    let name = "Отклонение частоты"
    let unit = "Гц"
    let data_to_string = Int32.to_string
  end
  module Bitrate : Measure_params with type t = int32 = struct
    type t   = int32
    let name = "Битрейт"
    let unit = "Бит/с"
    let data_to_string = Int32.to_string
  end
end

type typ = [ `Power | `Mer | `Ber | `Freq | `Bitrate ]

module Make_param(M:Measure_params) = struct
  type event = M.t option React.event
  let name   = M.name
  let val_to_string = function
    | Some v -> Printf.sprintf "%s %s" (M.data_to_string v) M.unit
    | None   -> "-"
  let get_name id = Printf.sprintf "Модуль %d. %s" (succ id) name

  class t ?(on_destroy:(unit -> unit) option) (event:event) (config:int) () =
    let name  = new Typography.Text.t
                    ~adjust_margin:false
                    ~font:Caption
                    ~text:(get_name config)
                    ()
    in
    let value = new Typography.Text.t
                    ~adjust_margin:false
                    ~font:Headline
                    ~text:(val_to_string None)
                    ()
    in
    let _  = React.E.map (fun v -> value#set_text @@ val_to_string v) event in
    object
      inherit Box.t ~vertical:false ~widgets:[name;value] () as super
      method! destroy = Option.iter (fun f -> f ()) on_destroy; super#destroy
    end

  let make ?on_destroy (event:event) (config:int) =
    new t ?on_destroy event config () |> Widget.coerce

end

module Power_param   = Make_param(Meas.Power)
module Mer_param     = Make_param(Meas.Mer)
module Ber_param     = Make_param(Meas.Ber)
module Freq_param    = Make_param(Meas.Freq)
module Bitrate_param = Make_param(Meas.Bitrate)

module Factory = struct

  type parameter_config = { id : int; typ : typ }

  (* Widget type *)
  type widget = Parameter of parameter_config
              | Chart

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
         let e = React.E.filter (fun (id,_) -> id = conf.id) self#get_measures in
         let on_destroy = fun () -> self#destroy_measures in
         (match conf.typ with
          | `Power   -> Power_param.make   ~on_destroy (React.E.map (fun (_,m) -> m.power) e)   conf.id
          | `Mer     -> Mer_param.make     ~on_destroy (React.E.map (fun (_,m) -> m.mer) e)     conf.id
          | `Ber     -> Ber_param.make     ~on_destroy (React.E.map (fun (_,m) -> m.ber) e)     conf.id
          | `Freq    -> Freq_param.make    ~on_destroy (React.E.map (fun (_,m) -> m.freq) e)    conf.id
          | `Bitrate -> Bitrate_param.make ~on_destroy (React.E.map (fun (_,m) -> m.bitrate) e) conf.id)
      | _ -> Widget.create @@ Dom_html.createDiv Dom_html.document

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
