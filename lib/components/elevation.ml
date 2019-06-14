include Components_tyxml.Elevation

let remove (w : #Widget.t) =
  List.iter (fun x ->
      if Utils.String.prefix ~pre:CSS.root x
      then w#remove_class x)
    w#classes

let set (w : #Widget.t) (x : int) =
  remove w;
  w#add_class CSS.transition;
  w#add_class @@ CSS.elevation x
