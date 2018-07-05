open Containers
open Components
open Widget_types
open Board_types

let base_class = "mdc-parameters-widget"

type config =
  { id: int
  } [@@deriving yojson]

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
        inherit Hbox.t ~widgets:[name#widget;value#widget] ()
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
  module Power      = Make(struct include Float      let typ = `Power   end)
  module Mer        = Make(struct include Float      let typ = `Mer     end)
  module Ber        = Make(struct include Scientific let typ = `Ber     end)
  module Freq       = Make(struct include Int        let typ = `Freq    end)
  module Bitrate    = Make(struct include Float      let typ = `Bitrate end)

end

let default_config = { id = 0 }

let name conf = let conf = Option.get_or ~default:default_config conf in
                Printf.sprintf "Модуль %d. Измерения" (succ conf.id)
let settings  = None

let make ~(measures:(int * measures) React.event) (config:config option) =
  let open Row in
  let open React in
  let config = Option.get_or ~default:default_config config in
  let measures = E.filter (fun (id,_) -> id = config.id) measures in
  let power   = Power.make (S.hold None @@ E.map (fun (_,m) -> m.power) measures) in
  let mer     = Mer.make   (S.hold None @@ E.map (fun (_,m) -> m.mer) measures) in
  let ber     = Ber.make   (S.hold None @@ E.map (fun (_,m) -> m.ber) measures) in
  let freq    = Freq.make  (S.hold None @@ E.map (fun (_,(m:measures)) -> m.freq) measures) in
  let bitrate = E.map (fun (_,m) -> Option.map (fun x -> float_of_int x /. 1_000_000.) m.bitrate) measures
                |> S.hold None |> Bitrate.make
  in
  let widget  = new Vbox.t ~widgets:[power;mer;ber;freq;bitrate] () in
  let ()      = widget#add_class base_class in
  widget#widget
