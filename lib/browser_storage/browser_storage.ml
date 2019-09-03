open Js_of_ocaml

type key = string

let put storage key json =
  storage##setItem
    (Js.string key)
    (Js.string @@ Yojson.Safe.to_string json)

let get storage key =
  Js.Opt.case (storage##getItem (Js.string key))
    (fun () -> None)
    (fun item ->
       try Some (Yojson.Safe.from_string (Js.to_string item))
       with _ -> None)

let remove storage key = storage##removeItem (Js.string key)

let clear storage = storage##clear

module type STORAGE = sig
  val is_available : bool

  val put : key -> Yojson.Safe.t -> unit

  val get : key -> Yojson.Safe.t option

  val remove : key -> unit

  val clear : unit -> unit
end

module Local : STORAGE = struct
  let storage = Dom_html.window##.localStorage

  let is_available = Js.Optdef.test storage

  let put k v = Js.Optdef.iter storage (fun s -> put s k v)

  let get k = Js.Optdef.case storage
      (fun () -> None)
      (fun s -> get s k)

  let remove k = Js.Optdef.iter storage (fun s -> remove s k)

  let clear () = Js.Optdef.iter storage clear
end

module Session : STORAGE = struct
  let storage = Dom_html.window##.sessionStorage

  let is_available = Js.Optdef.test storage

  let put k v = Js.Optdef.iter storage (fun s -> put s k v)

  let get k = Js.Optdef.case storage
      (fun () -> None)
      (fun s -> get s k)

  let remove k = Js.Optdef.iter storage (fun s -> remove s k)

  let clear () = Js.Optdef.iter storage clear
end

module type USER = sig
  type t

  val to_string : t -> string
end

module type USER_STORAGE = sig
  type user

  val is_available : bool

  val put : user -> key -> Yojson.Safe.t -> unit

  val get : user -> key -> Yojson.Safe.t option

  val remove : user -> key -> unit

  val clear : user -> unit
end

module Make (S : STORAGE) (U : USER) : USER_STORAGE with type user := U.t = struct
  let is_available = S.is_available

  let put user key data =
    let user = U.to_string user in
    let upd = match S.get user with
      | Some `Assoc l -> (key, data) :: l
      | _ -> [key, data]
    in
    S.put user (`Assoc upd)

  let get user key =
    match S.get (U.to_string user) with
    | Some `Assoc l -> List.assoc_opt key l
    | _ -> None

  let remove user key =
    let user = U.to_string user in
    let upd = match S.get user with
      | Some `Assoc l -> List.remove_assoc key l
      | _ -> []
    in
    S.put user (`Assoc upd)

  let clear user = S.remove (U.to_string user)
end

module Make_session = Make (Session)

module Make_local = Make (Local)
