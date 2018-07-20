open Containers
 
let match_streams
      (sources : (Common.Url.t * Common.Stream.t) list ref)
      (sl : Structure.structure list) : Structure.t list =
  let open Structure in
  let rec merge (sources : (Common.Url.t * Common.Stream.t) list) structure =
    match sources with
    | [] -> None (* TODO fix merging with unsaved streams *)
    | (uri, s)::ss ->
       if Common.Url.equal uri structure.uri
       then Some { source = s; structure }
       else merge ss structure
  in
  List.filter_map (merge !sources) sl

let dump_structures (entries : Structure.t list) =
  List.map (fun (x : Structure.t) -> (Yojson.Safe.to_string @@ Common.Stream.stream_id_to_yojson x.source.id)
                                   , (Yojson.Safe.to_string @@ Structure.structure_to_yojson x.structure))
    entries
