open Components
open Common

let make_factory (_ : Topology.t) : 'a #Dashboard.factory =
  object

    method create (_ : 'a) : Widget.t Dashboard.Item.item =
      failwith ""

    method destroy () : unit =
      ()

    method available =
      `List []

    method serialize (_ : 'a) : Yojson.Safe.json =
      `List []

    method deserialize (_ : Yojson.Safe.json) : ('a, string) result =
      Error ""

  end

class ['a] t () =
  let factory = make_factory (`Boards []) in
  object

    inherit ['a] Dashboard.t factory ()

  end

let make () : 'a t =
  new t ()
