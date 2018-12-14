open Containers

type user = [ `Root | `Operator | `Guest ]

let user_to_string = function
  | `Root -> "root"
  | `Operator -> "operator"
  | `Guest -> "guest"

let user_of_string = function
  | "root" -> Some `Root
  | "operator" -> Some `Operator
  | "guest" -> Some `Guest
  | _ -> None

type key = string [@@deriving yojson,eq]

type data = (key * Yojson.Safe.json) list [@@deriving yojson]

module type Store = sig
  val put : key -> Yojson.Safe.json -> unit
  val get : key -> Yojson.Safe.json option
  val remove : key -> unit
end

module Make_bs(M : sig val get_storage : unit -> Dom_html.storage Js.t option end) = struct
  let put key json = match M.get_storage () with
    | Some s -> s##setItem (Js.string key) (Js.string @@ Yojson.Safe.to_string json)
    | None   -> ()
  let get key = match M.get_storage () with
    | Some s ->
       begin match s##getItem (Js.string key) |> Js.Opt.to_option with
       | Some s -> (try Option.return @@ Yojson.Safe.from_string (Js.to_string s) with _ -> None)
       | None   -> None
       end
    | None -> None
  let remove key = match M.get_storage () with
    | Some s -> s##removeItem (Js.string key)
    | None -> ()
end

module Local =
  Make_bs(struct
      let get_storage () =
        Js.Optdef.to_option Dom_html.window##.localStorage
    end)
module Session =
  Make_bs(struct
      let get_storage () =
        Js.Optdef.to_option Dom_html.window##.sessionStorage
    end)

module type Storage = sig
  val put : key -> Yojson.Safe.json -> unit
  val get : key -> Yojson.Safe.json option
  val remove : key -> unit
  val clear : unit -> unit
end

module Make(M : Store) : Storage = struct

  let get_user () =
    let s = Js.to_string @@ Js.Unsafe.variable "username" in
    match user_of_string s with
    | Some u -> u
    | None -> failwith @@ Printf.sprintf "Unknown user type: %s" s

  let put (key : key) (data : Yojson.Safe.json) : unit =
    let user = get_user () in
    let upd  = match M.get @@ user_to_string user with
      | Some (`Assoc l) -> (key, data) :: l
      | _ -> [key, data]
    in
    M.put (user_to_string user) (`Assoc upd)

  let get (key : key) : Yojson.Safe.json option =
    let user = get_user () in
    match M.get @@ user_to_string user with
    | Some (`Assoc l) -> List.Assoc.get ~eq:equal_key key l
    | _ -> None

  let remove (key:key) : unit =
    let user = get_user () in
    let upd = match M.get @@ user_to_string user with
      | Some (`Assoc l) -> List.Assoc.remove ~eq:equal_key key l
      | _               -> []
    in
    M.put (user_to_string user) (`Assoc upd)

  let clear ()                =
    let user = get_user () in
    M.remove @@ user_to_string user

end

module Local_storage = Make(Local)
module Session_storage = Make(Session)
