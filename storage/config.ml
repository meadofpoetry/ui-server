type config = Yojson.Safe.json option
(*
let () =
  let _ = (let%lwt () = Lwt_io.printf "test\n" in
           Lwt.return_unit)
  in ()
 *)          
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
  type settings
  val  get      : t -> settings
end

module type DOMAIN = sig
  type t
  val domain    : string
  val default   : t
  val of_yojson : Yojson.Safe.json -> (t , string) Result.result
end

module Make (D : DOMAIN) : (CONFIG with type t := config and type settings := D.t) = struct
  let  get conf =
    let ( >>= ) = CCResult.( >>= ) in
    let rec find = function
      | [] -> Error ("Domain " ^ D.domain ^ " was not found")
      | (key, x)::_ when key = D.domain -> Ok x
      | (_, _)::tl -> find tl
    in
    match conf with
    | None -> D.default
    | Some cfg
      -> cfg |> function 
               | `Assoc lst
                 -> (find lst >>= D.of_yojson) |> (function Ok x -> x | Error _ -> D.default)
               | _ -> D.default
end
