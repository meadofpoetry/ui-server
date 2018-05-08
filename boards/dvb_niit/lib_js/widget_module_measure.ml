open Containers
open Components
open Widget_types

module type M = sig
  type t
  val to_string : t -> string
end

type config =
  { id  : int
  ; typ : Widget_types.measure_type
  } [@@deriving yojson]

module Make(M:M) = struct
  type event  = M.t option React.event

  let _class      = "mdc-parameter-widget"
  let inner_class = Markup.CSS.add_element _class "inner"

  let value_to_string (config:config) = function
    | Some v -> Printf.sprintf "%s %s" (M.to_string v) (measure_type_to_unit config.typ)
    | None   -> "-"
  let get_name (config:config) =
    let n = measure_type_to_string config.typ in
    Printf.sprintf "Модуль %d. %s" (succ config.id) n

  class t ?(on_destroy:(unit -> unit) option) (event:event) (config:config) () =
    let value = new Typography.Text.t
                    ~adjust_margin:false
                    ~font:Headline
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
      inherit Widget.widget box () as super
      method! destroy = Option.iter (fun f -> f ()) on_destroy; super#destroy
      initializer
        self#add_class _class
    end

  let make ?on_destroy (event:event) (config:config) : Dashboard.Item.item =
    { name     = get_name config
    ; settings = None
    ; widget   = (new t ?on_destroy event config ())#widget
    }

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
module Freq     = Make(Int32)
module Bitrate  = Make(Float)

let make ~(measures:Board_types.measure_response React.event)
         (config:config) =
  let open Board_types in
  let e = React.E.filter (fun (id,_) -> id = config.id) measures in
  (match config.typ with
   | `Power   -> Power.make   (React.E.map (fun (_,m) -> m.power) e)   config
   | `Mer     -> Mer.make     (React.E.map (fun (_,m) -> m.mer) e)     config
   | `Ber     -> Ber.make     (React.E.map (fun (_,m) -> m.ber) e)     config
   | `Freq    -> Freq.make    (React.E.map (fun (_,m) -> m.freq) e)    config
   | `Bitrate -> Bitrate.make (React.E.map (fun (_,m) ->
                                   Option.map (fun b -> Int32.to_float b /. 1_000_000.) m.bitrate) e) config)
