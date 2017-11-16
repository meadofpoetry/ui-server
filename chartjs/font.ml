module Obj = Base.Obj

type font =
    { family : string option
    ; size   : int option
    ; color  : string option
    ; style  : string option
    }

class type t =
  object
    method fontSize       : int Js.optdef_prop
    method fontStyle      : Js.js_string Js.t Js.optdef_prop
    method fontColor      : Js.js_string Js.t Js.optdef_prop (* FIXME to color type *)
    method fontFamily     : Js.js_string Js.t Js.optdef_prop
  end

let to_list ?prefix font =
  let f = (fun x -> match prefix with
                    | None   -> x
                    | Some s -> s ^ (String.capitalize_ascii x)) in
  Obj.cons_option (f "fontSize") font.size []
  |> Obj.map_cons_option ~f:Js.string (f "fontStyle") font.style
  |> Obj.map_cons_option ~f:Js.string (f "fontColor") font.color
  |> Obj.map_cons_option ~f:Js.string (f "fontFamily") font.family

let to_array ?prefix font =
  to_list ?prefix font |> Array.of_list
