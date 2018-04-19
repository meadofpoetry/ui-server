open Containers
open Components
open Widget_types
open Board_types

let base_class = "mdc-parameters-widget"

type config = { id: int }

module type M = sig
  type t
  val to_string : t -> string
  val typ : measure_type
end

module Row = struct
  module Make(M:M) = struct
    let _class = Markup.CSS.add_element base_class "row"
    class t ~(s_value:M.t option React.signal) () =
      let name  = new Typography.Text.t
                      ~adjust_margin:false
                      ~text:(measure_type_to_string M.typ)
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
                              | None   -> value#set_text "-") s_value
        inherit Box.t ~vertical:false ~widgets:[name#widget;value#widget] ()
        initializer
          self#add_class _class;
          name#add_class @@ Markup.CSS.add_element _class "name";
          value#add_class @@ Markup.CSS.add_element _class "value"
      end

    let make (s_value:M.t option React.signal) =
      new t ~s_value ()

  end

  module Float      = Widget_module_measure.Float
  module Scientific = Widget_module_measure.Scientific
  module Power      = Make(struct include Float let typ = `Power end)
  module Mer        = Make(struct include Float let typ = `Mer end)
  module Ber        = Make(struct include Scientific let typ = `Ber end)
  module Freq       = Make(struct include Int32 let typ = `Freq end)
  module Bitrate    = Make(struct include Float let typ = `Bitrate end)

end

let make ~(measures:Board_types.measure_response React.event) (config:config) =
  let open Row in
  let measures = React.E.filter (fun (id,_) -> id = config.id) measures
                 |> React.E.map snd
  in
  let power   = Power.make (React.S.hold None @@ React.E.map (fun m -> m.power) measures) in
  let mer     = Mer.make   (React.S.hold None @@ React.E.map (fun m -> m.mer) measures) in
  let ber     = Ber.make   (React.S.hold None @@ React.E.map (fun m -> m.ber) measures) in
  let freq    = Freq.make  (React.S.hold None @@ React.E.map (fun m -> m.freq) measures) in
  let bitrate = React.E.map (fun m -> Option.map (fun x -> Int32.to_float x /. 1_000_000.) m.bitrate) measures
                |> React.S.hold None
                |> Bitrate.make
  in
  let o = object(self)
            val mutable _name = Printf.sprintf "Модуль %d. Измерения" (succ config.id)
            inherit Box.t ~vertical:true ~widgets:[power;mer;ber;freq;bitrate] () as super
            method name = _name
            method set_name x = _name <- x
            method settings : unit Widget_grid.Item.settings option = None
            initializer
              self#add_class base_class
          end
  in (o :> unit Widget_grid.Item.t)
