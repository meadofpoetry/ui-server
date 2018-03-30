open Containers

let match_streams
      (sources : (Common.Uri.t * Common.Stream.t) list ref)
      (sl : Structure.structure list) : Structure.t list =
  let open Structure in
  let rec merge (sources : (Common.Uri.t * Common.Stream.t) list) structure =
    match sources with
    | [] -> { source = Unknown; structure }
    | (uri, s)::ss ->
       if Common.Uri.equal uri structure.uri
       then { source = Stream s; structure }
       else merge ss structure
  in
  List.map (merge !sources) sl

let dump_streams (entries : Structure.t list) =
  let open Structure in
  List.map (fun prog -> (prog.structure.uri, Msg_conv.to_string @@ structure_to_yojson prog.structure)) entries
