open Containers
open Components
open Widget_types
open Board_types
open Common

type config =
  { id : Stream.ID.t
  ; typ : Widget_types.measure_type
  } [@@deriving yojson]

let base_class = "dvb-niit-module-measure"

module Make(M:sig type t val to_string : t -> string end) = struct
  type event  = M.t option React.event

  let inner_class = Markup.CSS.add_element base_class "inner"

  let value_to_string (config:config) = function
    | Some v -> Printf.sprintf "%s %s" (M.to_string v) (measure_type_to_unit config.typ)
    | None   -> "-"

  class t (event:event) (config:config) () =
    let value =
      new Typography.Text.t
        ~adjust_margin:false
        ~font:Headline_5
        ~text:(value_to_string config None)
        () in
    let _e    = React.E.map (fun v -> value#set_text
                                      @@ value_to_string config v) event in
    let inner = Widget.create_div () in
    object(self)
      inherit Widget.t Js_of_ocaml.Dom_html.(createDiv document) ()
      initializer
        self#_keep_e _e;
        self#append_child inner;
        self#add_class base_class;
        inner#append_child value;
        inner#add_class inner_class;
    end

  let make (event:event) (config:config) =
    new t event config () |> Widget.coerce

end

module Float = struct
  type t = float
  let to_string t = Printf.sprintf "%.3g" t
end
module Scientific = struct
  type t = float
  let to_string t = Printf.sprintf "%.3e" t
end
module Power = Make(Float)
module Mer = Make(Float)
module Ber = Make(Scientific)
module Freq = Make(Int)
module Bitrate = Make(Float)

let name conf =
  let n = measure_type_to_string conf.typ in
  n

let settings  = None

let equal_id = Stream.ID.equal

let make ~(measures : (Stream.t * Measure.t Time.timestamped) React.event)
      (config : config) =
  let open React in
  let open Measure in
  let e = E.fmap (fun ((s : Stream.t), (x : Measure.t Time.timestamped)) ->
              if equal_id s.id config.id
              then Some x.data else None) measures in
  (match config.typ with
   | `Power -> Power.make (E.map (fun m -> m.power) e) config
   | `Mer -> Mer.make (E.map (fun m -> m.mer) e) config
   | `Ber -> Ber.make (E.map (fun m -> m.ber) e) config
   | `Freq -> Freq.make (E.map (fun m -> m.freq) e) config
   | `Bitrate ->
      Bitrate.make (
          E.map (fun m ->
              Option.map (fun b -> float_of_int b /. 1_000_000.)
                m.bitrate) e) config)
