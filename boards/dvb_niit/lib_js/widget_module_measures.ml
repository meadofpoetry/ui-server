open Containers
open Components
open Widget_types
open Board_types

let base_class = "dvb-niit-module-measures"

type config =
  { id: int
  } [@@deriving yojson]

module type M = sig
  type t
  val to_string : t -> string
  val typ : measure_type
end

module Row = struct

  module Make(M : M) = struct
    let _class = Markup.CSS.add_element base_class "row"
    class t ~(s_value : M.t option React.signal) () =
      let meta =
        new Typography.Text.t
          ~adjust_margin:false
          ~text:""
          () in
      object
        val _s =
          React.S.map (function
              | Some v ->
                 let s = M.to_string v ^ " " ^ measure_type_to_unit M.typ in
                 meta#set_text s
              | None   -> meta#set_text "-") s_value
        inherit [unit] Item_list.Item.t
                  ~meta
                  ~text:(measure_type_to_string M.typ)
                  ~value:()
                  ()
      end

    let make (s_value:M.t option React.signal) =
      new t ~s_value ()

  end

  module Float = Widget_module_measure.Float
  module Scientific = Widget_module_measure.Scientific
  module Power = Make(struct include Float let typ = `Power end)
  module Mer = Make(struct include Float let typ = `Mer end)
  module Ber = Make(struct include Scientific let typ = `Ber end)
  module Freq = Make(struct include Int let typ = `Freq end)
  module Bitrate = Make(struct include Float let typ = `Bitrate end)

end

let default_config = { id = 0 }

let name conf =
  let conf = Option.get_or ~default:default_config conf in
  Printf.sprintf "Модуль %d. Измерения" (succ conf.id)
let settings  = None

let make ~(measures : (int * Measure.t) React.event)
      (config : config option) =
  let open Row in
  let open React in
  let open Measure in
  let config = Option.get_or ~default:default_config config in
  let measures = E.filter (fun (id,_) -> id = config.id) measures in
  let power = Power.make (S.hold None @@ E.map (fun (_, m) -> m.power) measures) in
  let mer = Mer.make (S.hold None @@ E.map (fun (_, m) -> m.mer) measures) in
  let ber = Ber.make (S.hold None @@ E.map (fun (_, m) -> m.ber) measures) in
  let freq = Freq.make (S.hold None @@ E.map (fun (_, m) -> m.freq) measures) in
  let bitrate =
    E.map (fun (_, m) ->
        Option.map (fun x -> float_of_int x /. 1_000_000.) m.bitrate) measures
    |> S.hold None |> Bitrate.make in
  let items = [power; mer; ber; freq; bitrate] in
  let list  =
    new Item_list.t
      ~items:(List.map (fun x -> `Item x) items)
      ~non_interactive:true
      () in
  list#add_class base_class;
  list#widget
