open Base
open Containers

module Piece_label = Piece_label

class type t_js =
  object
    inherit Options.t_js
    method cutoutPercentage : Js.number Js.t Js.prop
    method rotation         : Js.number Js.t Js.prop
    method circumference    : Js.number Js.t Js.prop
    (* Extensions *)
    method pieceLabel       : Piece_label.t_js Js.t Js.optdef_prop
  end

class t ?piece_label () =
  let o : t_js Js.t = Js.Unsafe.coerce @@ Js.Unsafe.obj [||] in
  object

    inherit Options.t o () as super

    method! replace x =
      super#replace x;
      Option.iter (fun (x:Piece_label.t) ->
          Js.Optdef.iter _obj##.pieceLabel (fun pl -> x#replace pl))
        piece_label

    initializer
      Option.iter (fun (x:Piece_label.t) ->
          _obj##.pieceLabel := Js.Unsafe.coerce x#get_obj) piece_label;

  end
