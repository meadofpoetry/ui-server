open Pipeline_types

type halign = Left | HCenter | Right
type valign = Top | VCenter | Bottom
type align = halign * valign

let min_size (container : Wm.container) =
  failwith "TODO"

let scale (container : Wm.container)
    (width, height)
    (align : align) =
  failwith "TODO"
