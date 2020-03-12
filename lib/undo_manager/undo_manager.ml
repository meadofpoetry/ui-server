open Js_of_ocaml

type v = { undo : unit -> unit; redo : unit -> unit }

type t = {
  mutable stack : v Js.js_array Js.t;
  mutable limit : int option;
  mutable index : int;
  mutable callback : (t -> unit) option;
}

let create ?callback ?limit () : t =
  { stack = new%js Js.array_empty; index = -1; limit; callback }

(** Add an action to the stack *)
let add v x =
  let (_ : v Js.js_array Js.t) =
    v.stack##splice (v.index + 1) (v.stack##.length - v.index)
  in
  let (_ : int) = v.stack##push x in
  (* If limit is set, remove items from start. *)
  ( match v.limit with
  | None -> ()
  | Some limit ->
      if v.stack##.length > limit then
        let length = v.stack##.length in
        v.stack <- v.stack##slice (length - limit) length );
  (* Set the index to the end *)
  v.index <- v.stack##.length - 1;
  match v.callback with None -> () | Some f -> f v

(** Perform undo *)
let undo v =
  let cmd = Js.array_get v.stack v.index in
  Js.Optdef.iter cmd (fun cmd ->
      cmd.undo ();
      v.index <- v.index - 1;
      match v.callback with None -> () | Some f -> f v)

(** Perform redo *)
let redo v =
  let cmd = Js.array_get v.stack (v.index + 1) in
  Js.Optdef.iter cmd (fun cmd ->
      cmd.redo ();
      v.index <- v.index + 1;
      match v.callback with None -> () | Some f -> f v)

let clear v =
  let size = v.stack##.length in
  v.stack <- new%js Js.array_empty;
  v.index <- -1;
  match (size, v.callback) with x, Some f when x > 0 -> f v | _ -> ()

let has_undo v = v.index <> -1

let has_redo v = v.index < v.stack##.length - 1

let set_callback v cb = v.callback <- Some cb
