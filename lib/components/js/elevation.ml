include Components_tyxml.Elevation

let remove elt =
  List.iter (fun class' ->
      match BEM.get_block class' with
      | Some s when String.equal s CSS.root -> Element.remove_class elt class'
      | _ -> ())
  @@ Element.classes elt

let set elt (x : int) =
  remove elt;
  Element.add_class elt CSS.transition;
  Element.add_class elt (CSS.elevation x)
