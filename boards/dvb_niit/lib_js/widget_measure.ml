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
  val make : int -> t option React.signal -> Widget.widget
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
  include Widget_module_measure.M
  val get : int -> measure_response React.event -> t option React.event
end

module Make(M:M) = struct

  let make (event:measure_response React.event)
           (config:(Board_types.config React.signal,string) Lwt_result.t)
           (conf:config) : Dashboard.Item.item =
    let (module R)   = (module struct include M let typ = conf.typ end : Row.M with type t = M.t) in
    let (module ROW) = (module struct include Row.Make(R) end : R with type t = M.t) in
    let t = match conf.ids with
      | Some l -> Lwt_result.return l
      | None   -> config >>= (fun c -> Lwt_result.return @@ List.map fst @@ React.S.value c)
    in
    let t = t >>= fun ids ->
            let rows = List.map (fun id -> ROW.make id @@ React.S.hold None @@ M.get id event)
                       @@ List.sort compare ids in
            (new Box.t ~vertical:true ~widgets:rows ())
            |> Widget.coerce
            |> Lwt_result.return
    in
    let widget = object(self)
                   inherit Ui_templates.Loader.widget_loader t ()
                   initializer
                     self#add_class base_class
                 end
    in
    { name     = Printf.sprintf "Измерения. %s" @@ measure_type_to_string conf.typ
    ; settings = None
    ; widget   = widget#widget
    }

end

module Float      = Widget_module_measure.Float
module Scientific = Widget_module_measure.Scientific
module Power      = Make(struct
                          include Float
                          let get x e = React.E.filter (fun (id,_) -> id = x) e
                                        |> React.E.map (fun (_,m)  -> m.power)
                        end)
module Mer        = Make(struct
                          include Float
                          let get x e = React.E.filter (fun (id,_) -> id = x) e
                                        |> React.E.map (fun (_,m)  -> m.mer)
                        end)
module Ber        = Make(struct
                          include Float
                          let get x e = React.E.filter (fun (id,_) -> id = x) e
                                        |> React.E.map (fun (_,m)  -> m.ber)
                        end)
module Freq       = Make(struct
                          include Int32
                          let get x e = React.E.filter (fun (id,_) -> id = x) e
                                        |> React.E.map (fun (_,m)  -> m.freq)
                        end)
module Bitrate    = Make(struct
                          include Float
                          let get x (e:measure_response React.event) =
                            React.E.filter (fun (id,_) -> id = x) e
                            |> React.E.map (fun (_,m)  -> Option.map (fun x -> (Int32.to_float x) /. 1_000_000.)
                                                                     m.bitrate)
                        end)

let make ~(measures:Board_types.measure_response React.event)
         ~(config:(Board_types.config React.signal,string) Lwt_result.t)
         (conf:config) =
  match conf.typ with
  | `Power   -> Power.make   measures config conf
  | `Mer     -> Mer.make     measures config conf
  | `Ber     -> Mer.make     measures config conf
  | `Freq    -> Freq.make    measures config conf
  | `Bitrate -> Bitrate.make measures config conf
