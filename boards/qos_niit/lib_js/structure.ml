open Containers
open Components
open Board_types
open Lwt_result.Infix

let page control () =
  let factory = new Widget_factory.t control () in
  let i = new Dashboard.Item.t ~item:(factory#create (Structure None)) () in
  object
    inherit Widget.t i#root () as super
    method destroy () = super#destroy (); factory#destroy ()
  end
