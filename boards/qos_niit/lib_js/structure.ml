open Containers
open Common
open Components
open Board_types.Streams.TS
open Lwt_result.Infix

let structs (signal:(Stream.id * structure option) list React.signal) =
  ()

(* let map_s (s:(Stream.id * structure) list React.signal) =
 *   React.S.fold (fun acc x ->
 *       let acc = List.map (fun )
 *       acc) (React.S.value s) (React.S.changes s) *)

let page control () =
  let factory = new Widget_factory.t control () in
  let i = factory#create (Structure None) in
  object
    inherit Widget.t i.widget#root () as super
    method destroy () = super#destroy (); factory#destroy ()
  end
