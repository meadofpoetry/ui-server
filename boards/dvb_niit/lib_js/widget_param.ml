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
  }

module Make(M:M) = struct
  type event  = M.t option React.event

  let value_to_string (config:config) = function
    | Some v -> Printf.sprintf "%s %s" (M.to_string v) (measure_type_to_unit config.typ)
    | None   -> "-"
  let get_name (config:config) =
    let n = measure_type_to_string config.typ in
    Printf.sprintf "Модуль %d. %s" (succ config.id) n

  class t ?(on_destroy:(unit -> unit) option) (event:event) (config:config) () =
    let name  = new Typography.Text.t
                    ~adjust_margin:false
                    ~font:Caption
                    ~text:(get_name config)
                    ()
    in
    let value = new Typography.Text.t
                    ~adjust_margin:false
                    ~font:Headline
                    ~text:(value_to_string config None)
                    ()
    in
    let _  = React.E.map (fun v -> value#set_text @@ value_to_string config v) event in
    let card = new Card.t ~widgets:[name;value] () in
    object
      inherit Widget.widget card#root () as super
      method! destroy = Option.iter (fun f -> f ()) on_destroy; super#destroy
    end

  let make ?on_destroy (event:event) (config:config) =
    new t ?on_destroy event config () |> Widget.coerce

end
module Float = struct
  type t = float
  let to_string t = string_of_float t |> (fun x -> if String.suffix ~suf:"." x then x ^ "0" else x)
end
module Power    = Make(Float)
module Mer      = Make(Float)
module Ber      = Make(Float)
module Freq     = Make(Int32)
module Bitrate  = Make(Int32)
