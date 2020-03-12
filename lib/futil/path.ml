type t = string

type error = [ `Bad_path of string ]

let pp_error ppf (`Bad_path msg) = Fmt.fmt "bad path %s" ppf msg

let () = assert (Filename.dir_sep = "/")

let string_has c s =
  try
    String.iter (fun x -> if x = c then raise_notrace Exit) s;
    false
  with Exit -> true

let of_implicit s =
  let replace_tilda = function "~" :: tl -> "$HOME" :: tl | path -> path in
  let replace_vars =
    let get_var s =
      let s = if s.[0] = '$' then String.sub s 1 (String.length s - 1) else s in
      Sys.getenv s
    in
    List.map (function
      | s when String.length s > 0 && s.[0] = '$' -> get_var s
      | s -> s)
  in
  let to_explicit path =
    match path with
    | h :: _ when Filename.is_relative h -> Sys.getcwd () :: path
    | _ -> path
  in
  let set_path = String.split_on_char '/' s in
  try
    set_path
    |> replace_tilda
    |> replace_vars
    |> to_explicit
    |> List.filter (fun x -> String.length x <> 0)
    |> String.concat Filename.dir_sep
    |> fun x -> Ok x
  with e -> Error (`Bad_path (Printexc.to_string e))

let of_string s =
  if Filename.is_implicit s || string_has '$' s then of_implicit s else Ok s

let to_string x = x

let to_explicit_exn x =
  match of_string x with
  | Error (`Bad_path e) -> failwith e
  | Ok v -> to_string v

let append path l = Filename.concat path (String.concat Filename.dir_sep l)
