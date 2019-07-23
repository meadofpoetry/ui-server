(*
open Netlib
open Application_types
open Pipeline_types
   
(* TODO remove 4.08 *)
let filter_map f l =
  let rec loop acc = function
    | [] -> List.rev acc
    | x::tl ->
       match f x with
       | None -> loop acc tl
       | Some v -> loop (v::acc) tl
  in loop [] l
   
let match_streams
      (sources : (Uri.t * Stream.t) list)
      (sl : Structure.t list) : Structure.Packed.t list =
  let open Structure in
  let rec merge (sources : (Uri.t * Stream.t) list) structure =
    match sources with
    | [] -> None (* TODO fix merging with unsaved streams *)
    | (uri, s)::ss ->
       if Uri.equal uri structure.uri
       then Some Packed.{ source = s; structure }
       else merge ss structure
  in
  filter_map (merge sources) sl

let dump_structures (entries : Structure.Packed.t list) =
  List.map (fun (x : Structure.Packed.t) ->
      (Yojson.Safe.to_string @@ Stream.ID.to_yojson x.source.id),
      (Yojson.Safe.to_string @@ Structure.to_yojson x.structure))
    entries
  *)
