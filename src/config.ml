type t = Yojson.Safe.json option

let create path =
  try 
    Unix.stat path
    |> (fun st -> st.Unix.st_kind)
    |> function
      | Unix.S_REG -> Some (Yojson.Safe.from_file path)
      | _ -> None
  with _ -> None
                    
module type CONFIG = sig
  type t
  val  get : (Yojson.Safe.json -> ('a , string) Result.result) -> string -> 'a -> t -> 'a
end

let get of_json domain default cfg =
  let ( >>= ) = CCResult.( >>= ) in
  let rec find = function
    | [] -> Error ("Domain " ^ domain ^ " was not found")
    | (key, x)::_ when key = domain -> Ok x
    | (_, _)::tl -> find tl
  in
  match cfg with
  | None -> default
  | Some cfg
    -> cfg |> function 
             | `Assoc lst
               -> (find lst >>= of_json) |> (function Ok x -> x | Error _ -> default)
             | _ -> default
