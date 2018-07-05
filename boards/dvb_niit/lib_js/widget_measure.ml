open Containers
open Components
open Widget_types
open Board_types
open Lwt_result.Infix

let base_class = "mdc-parameters-widget"

type config =
  { ids : int list option
  ; typ : measure_type
  } [@@deriving yojson]

module type R = sig
  type t
  val make : int -> t option React.signal -> Widget.t
end

module Row = struct
  module type M = Widget_module_measures.M
  module Make(M:M) : R with type t = M.t = struct

    type t = M.t

    let _class = Markup.CSS.add_element base_class "row"
    class w id s () =
      let name = new Typography.Text.t
                     ~adjust_margin:false
                     ~text:(Printf.sprintf "Модуль %d" @@ succ id)
                     ()
      in
      let value = new Typography.Text.t
                      ~adjust_margin:false
                      ~text:""
                      ()
      in
      object(self)
        val _s = React.S.map (function
                              | Some v -> value#set_text @@ M.to_string v ^ " " ^ measure_type_to_unit M.typ
                              | None   -> value#set_text "-") s
        inherit Box.t ~vertical:false ~widgets:[name#widget;value#widget] ()
        initializer
          self#add_class _class;
          name#add_class @@ Markup.CSS.add_element _class "name";
          value#add_class @@ Markup.CSS.add_element _class "value"
      end

    let make (id:int) (s:M.t option React.signal) =
      new w id s () |> Widget.coerce

  end

end

module type M = sig
  type t
  val to_string : t -> string
  val get : int -> (int * measures) React.event -> t option React.event
end

module Make(M:M) = struct

  let make (event:(int * measures) React.event)
           (config:Board_types.config React.signal)
           (conf:config) =
    let (module R)   = (module struct include M let typ = conf.typ end : Row.M with type t = M.t) in
    let (module ROW) = (module struct include Row.Make(R) end : R with type t = M.t) in
    let ids = match conf.ids with Some x -> x | None -> List.map fst @@ React.S.value config in
    let rows = List.map (fun id -> ROW.make id @@ React.S.hold None @@ M.get id event)
               @@ List.sort compare ids in
    let box = new Box.t ~vertical:true ~widgets:rows () in
    let ()  = box#add_class base_class in
    box#widget

end

module Float      = Widget_module_measure.Float
module Scientific = Widget_module_measure.Scientific
module Power      = Make(struct
                        include Float
                        let get x e = React.(E.filter (fun (id,_) -> id = x) e
                                             |> E.map (fun (_,m)  -> m.power))
                      end)
module Mer        = Make(struct
                        include Float
                        let get x e = React.(E.filter (fun (id,_) -> id = x) e
                                             |> E.map (fun (_,m)  -> m.mer))
                      end)
module Ber        = Make(struct
                        include Float
                        let get x e = React.(E.filter (fun (id,_) -> id = x) e
                                             |> E.map (fun (_,m)  -> m.ber))
                      end)
module Freq       = Make(struct
                        include Int
                        let get x e = React.(E.filter (fun (id,_) -> id = x) e
                                             |> E.map (fun (_,(m:measures))  -> m.freq))
                      end)
module Bitrate    = Make(struct
                        include Float
                        let get x e =
                          React.(E.filter (fun (id,_) -> id = x) e
                                 |> E.map (fun (_,m)  -> Option.map (fun x -> float_of_int x /. 1_000_000.)
                                                           m.bitrate))
                      end)

let default_config = { ids = None; typ = `Power }

let name conf = Printf.sprintf "Измерения. %s" @@ measure_type_to_string
                @@ (Option.get_or ~default:default_config conf).typ
let settings  = None

let make ~(measures:(int * measures) React.event)
         ~(config:Board_types.config React.signal)
         (conf:config option) =
  let conf = Option.get_or ~default:default_config conf in (* FIXME *)
  match conf.typ with
  | `Power   -> Power.make   measures config conf
  | `Mer     -> Mer.make     measures config conf
  | `Ber     -> Mer.make     measures config conf
  | `Freq    -> Freq.make    measures config conf
  | `Bitrate -> Bitrate.make measures config conf
