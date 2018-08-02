open Containers
open Components
open Widget_types
open Board_types

type config =
  { id  : int
  ; typ : Widget_types.measure_type
  } [@@deriving yojson]

module Make(M:sig type t val to_string : t -> string end) = struct
  type event  = M.t option React.event

  let _class      = "mdc-parameter-widget"
  let inner_class = Markup.CSS.add_element _class "inner"

  let value_to_string (config:config) = function
    | Some v -> Printf.sprintf "%s %s" (M.to_string v) (measure_type_to_unit config.typ)
    | None   -> "-"

  class t (event:event) (config:config) () =
    let value = new Typography.Text.t
                    ~adjust_margin:false
                    ~font:Headline_5
                    ~text:(value_to_string config None)
                    ()
    in
    let _     = React.E.map (fun v -> value#set_text @@ value_to_string config v) event in
    let box   = Dom_html.createDiv Dom_html.document in
    let inner = Dom_html.createDiv Dom_html.document |> Widget.create in
    let ()    = inner#add_class inner_class in
    let ()    = Dom.appendChild inner#root value#root in
    let ()    = Dom.appendChild box inner#root in
    object(self)
      inherit Widget.t box () as super
      initializer
        self#add_class _class
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
module Power    = Make(Float)
module Mer      = Make(Float)
module Ber      = Make(Scientific)
module Freq     = Make(Int)
module Bitrate  = Make(Float)

let default_config = { id = 0; typ = `Power }

let name conf = let conf = Option.get_or ~default:default_config conf in
                let n = measure_type_to_string conf.typ in
                Printf.sprintf "Модуль %d. %s" (succ conf.id) n
let settings  = None

let make ~(measures:(int * measures) React.event)
         (config:config option) =
  let config = Option.get_or ~default:default_config config in
  let e = React.E.filter (fun (id,_) -> id = config.id) measures in
  (match config.typ with
   | `Power   -> Power.make   (React.E.map (fun (_,m) -> m.power) e)   config
   | `Mer     -> Mer.make     (React.E.map (fun (_,m) -> m.mer) e)     config
   | `Ber     -> Ber.make     (React.E.map (fun (_,m) -> m.ber) e)     config
   | `Freq    -> Freq.make    (React.E.map (fun (_,(m:measures)) -> m.freq) e)    config
   | `Bitrate -> Bitrate.make (React.E.map (fun (_,m) ->
                                   Option.map (fun b -> float_of_int b /. 1_000_000.) m.bitrate) e) config)
