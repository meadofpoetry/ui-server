open Containers

let match_streams
      (sources : (Common.Uri.t * Common.Stream.t) list ref)
      (sl : Structure.structure list) : Structure.t list =
  let open Structure in
  let rec merge (sources : (Common.Uri.t * Common.Stream.t) list) structure =
    match sources with
    | [] -> raise Not_found
    | (uri, s)::ss ->
       if Common.Uri.equal uri structure.uri
       then { source = s; structure }
       else merge ss structure
  in
  List.map (merge !sources) sl

module Map_input = Map.Make(struct
                       open Common.Topology
                       type t = Common.Topology.topo_input
                       let compare l r =
                         let c = Common.Topology.input_compare l.input r.input in
                         if c <> 0 then c
                         else compare l.id r.id
                     end)
  
let sort_streams_by_inputs (entries : Structure.t list) =
  let open Structure in
  List.fold_left (fun map package ->
      let input = Common.Stream.get_input package.source in
      Map_input.update input (function None -> Some [package] | Some l -> Some (package::l)) map)
    Map_input.empty entries
  |> Map_input.to_list
       
let dump_structures (entries : Structure.t list) =
  sort_streams_by_inputs entries
  |> List.map (fun (i,s) -> (Yojson.Safe.to_string @@ Common.Topology.topo_input_to_yojson i)
                          , (Yojson.Safe.to_string @@ Structure.Streams.to_yojson s))
