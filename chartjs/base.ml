[@@@ocaml.warning "-60"]
open Containers

(* TODO remove *)
let (=) = Pervasives.(=)
   
let (%>) = Fun.(%>)

module Cast = struct

  let to_bool x : bool option =
    if Js.typeof x = (Js.string "boolean")
    then Some (Js.to_bool @@ Js.Unsafe.coerce x)
    else None

  let to_int x : int option =
    if Js.typeof x = (Js.string "number")
    then Some (int_of_float @@ Js.float_of_number (Js.Unsafe.coerce x))
    else None

  let to_float x : float option =
    if Js.typeof x = (Js.string "number")
    then Some (Js.float_of_number (Js.Unsafe.coerce x))
    else None

  let to_js_string x : Js.js_string Js.t option =
    if Js.typeof x = (Js.string "string")
    then Some (Js.Unsafe.coerce x)
    else None

  let to_string x : string option =
    Option.map Js.to_string @@ to_js_string x

  let to_list ~(f:'a -> 'c) (x:'b Js.t) : 'c list option =
    let array_constr : 'a Js.js_array Js.t Js.constr = Js.Unsafe.global##._Array in
    if Js.instanceof x array_constr
    then Some (Array.to_list @@ Js.to_array (Js.Unsafe.coerce x)
               |> List.map f)
    else None

  let to_js_array x : 'a Js.js_array Js.t option =
    let array_constr : 'a Js.js_array Js.t Js.constr = Js.Unsafe.global##._Array in
    if Js.instanceof x array_constr
    then Some (Js.Unsafe.coerce x)
    else None

  let to_color x : CSS.Color.t option =
    Option.map (fun x -> CSS.Color.ml @@ CSS.Color.js_t_of_js_string x) @@ to_js_string x

  let to_object x =
    if Js.typeof x = (Js.string "object")
    then Some (Js.Unsafe.coerce x)
    else None

end

module Canvas = struct

  type line_cap = Butt | Round | Square

  type line_join = Bevel | Round | Miter

  let line_cap_to_string = function
    | Butt -> "butt" | Round -> "round" | Square -> "square"
  let line_cap_of_string_exn = function
    | "butt" -> Butt | "round" -> Round | "square" -> Square | _ -> failwith "Bad line cap string"

  let line_join_to_string = function
    | Bevel -> "bevel" | Round -> "round" | Miter -> "miter"
  let line_join_of_string_exn = function
    | "bevel" -> Bevel | "round" -> Round | "miter" -> Miter | _ -> failwith "Bad line join string"

end

module Obj = struct
  let (>|=) x f = Js.Optdef.map x f
  let map x f   = Js.Optdef.option x >|= f |> Js.Unsafe.inject
  let wrap x    = Js.Optdef.option x |> Js.Unsafe.inject

  type 'a opt_field = string * 'a

  let map_cons_option ~f name opt l = Option.map_or ~default:l (fun x -> (name, Js.Unsafe.inject @@ f x) :: l) opt
  let cons_option name opt l        = Option.map_or ~default:l (fun x -> (name, Js.Unsafe.inject x) :: l) opt

  let append_option opt l = Option.map_or ~default:l (fun x -> x @ l) opt
  let map_append_option ~f opt l = Option.map_or ~default:l (fun x -> (f x) @ l) opt

end

type interaction_mode = Point
                      | Nearest
                      | Index
                      | Dataset
                      | X
                      | Y

let interaction_mode_to_string = function
  | Point   -> "point"   | Nearest -> "nearest" | Index   -> "index"
  | Dataset -> "dataset" | X       -> "x"       | Y       -> "y"
let interaction_mode_of_string_exn = function
  | "point"   -> Point   | "nearest" -> Nearest | "index" -> Index
  | "dataset" -> Dataset | "x"       -> X       | "y"     -> Y
  | _ -> failwith "Bad inderaction mode string"

type easing = Linear
            | Ease_in of animation_type
            | Ease_out of animation_type
            | Ease_in_out of animation_type
 and animation_type = Quad
                    | Cubic
                    | Quart
                    | Quint
                    | Sine
                    | Expo
                    | Circ
                    | Elastic
                    | Back
                    | Bounce

let animation_type_to_string = function
  | Quad -> "Quad" | Cubic  -> "Cubic" | Quart -> "Quart" | Quint   -> "Quint"
  | Sine -> "Sine" | Expo   -> "Expo"  | Circ  -> "Circ"  | Elastic -> "Elastic"
  | Back -> "Back" | Bounce -> "Bounce"
let animation_type_of_string_exn = function
  | "Quad" -> Quad | "Cubic"  -> Cubic  | "Quart" -> Quart | "Quint"   -> Quint
  | "Sine" -> Sine | "Expo"   -> Expo   | "Circ"  -> Circ  | "Elastic" -> Elastic
  | "Back" -> Back | "Bounce" -> Bounce | _ -> failwith "Bad animation type string"

let easing_to_string = function
  | Linear        -> "linear"
  | Ease_in x     -> "easeIn" ^ animation_type_to_string x
  | Ease_out x    -> "easeOut" ^ animation_type_to_string x
  | Ease_in_out x -> "easeInOut" ^ animation_type_to_string x

let easing_of_string_exn x =
  let in_s     = "easeIn" in
  let out_s    = "easeOut" in
  let in_out_s = "easeInOut" in
  let f pre s =
    let get_typ rest = (match pre with
                        | x when x = in_s     -> Ease_in (animation_type_of_string_exn rest)
                        | x when x = out_s    -> Ease_out (animation_type_of_string_exn rest)
                        | x when x = in_out_s -> Ease_in_out (animation_type_of_string_exn rest)
                        | _ -> failwith "Bad easing prefix value") in
    match String.chop_prefix ~pre s with
    | Some rest -> get_typ rest
    | None      -> failwith "Bad easing prefix value" in
  match x with
  | "linear" -> Linear
  | s when String.prefix ~pre:in_s s     -> f in_s s
  | s when String.prefix ~pre:out_s s    -> f out_s s
  | s when String.prefix ~pre:in_out_s s -> f in_out_s s
  | _ -> failwith "Bad easing string"

type typ = Line
         | Bar
         | Radar
         | Pie
         | Doughnut
         | Polar
         | Bubble
         | Scatter

let typ_to_string = function
  | Line   -> "line"   | Bar      -> "bar"      | Radar -> "radar"
  | Pie    -> "pie"    | Doughnut -> "doughnut" | Polar -> "polarArea"
  | Bubble -> "bubble" | Scatter  -> "scatter"

class ['a] base_option () = object
  val mutable obj : 'a Js.t = Js.Unsafe.obj [||]

  method get_obj   = obj
  method replace x = obj <- x
end
