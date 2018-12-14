open Containers
open Common
 
let match_streams
      (sources : (Url.t * Stream.t) list ref)
      (sl : Structure.t list) : Structure.packed list =
  let open Structure in
  let rec merge (sources : (Url.t * Stream.t) list) structure =
    match sources with
    | [] -> None (* TODO fix merging with unsaved streams *)
    | (uri, s)::ss ->
       if Common.Url.equal uri structure.uri
       then Some { source = s; structure }
       else merge ss structure
  in
  CCList.filter_map (merge !sources) sl

let dump_structures (entries : Structure.packed list) =
  List.map (fun (x : Structure.packed) ->
      (Yojson.Safe.to_string @@ Stream.ID.to_yojson x.source.id),
      (Yojson.Safe.to_string @@ Structure.to_yojson x.structure))
    entries
