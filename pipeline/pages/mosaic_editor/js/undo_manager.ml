open Js_of_ocaml

type v =
  { undo : unit -> unit
  ; redo : unit -> unit
  }

type t =
  { mutable stack : v Js.js_array Js.t
  ; mutable limit : int
  ; mutable index : int
  ; mutable callback : (t -> unit) option
  }

let create ?callback ?(limit = 100) () : t =
  { stack = new%js Js.array_empty
  ; index = -1
  ; limit = 0
  ; callback
  }

(** Add an action to the stack *)
let add v x =
  let (_ : v Js.js_array Js.t) = v.stack##splice
      (v.index + 1)
      (v.stack##.length - v.index) in
  let (_ : int) = v.stack##push x in
  if v.limit > 0 && v.stack##.length > v.limit
  then ();
  (* Set the index to the end *)
  v.index <- v.stack##.length - 1;
  match v.callback with
  | None -> ()
  | Some f -> f v

(** Perform undo *)
let undo v =
  let cmd = Js.array_get v.stack v.index in
  Js.Optdef.iter cmd (fun cmd ->
      cmd.undo ();
      v.index <- v.index - 1;
      match v.callback with
      | None -> ()
      | Some f -> f v)

(** Perform redo *)
let redo v =
  let cmd = Js.array_get v.stack (v.index + 1) in
  Js.Optdef.iter cmd (fun cmd ->
      cmd.redo ();
      v.index <- v.index + 1;
      match v.callback with
      | None -> ()
      | Some f -> f v)

let clear v =
  let size = v.stack##.length in
  v.stack <- new%js Js.array_empty;
  v.index <- -1;
  match size, v.callback with
  | x, Some f when x > 0 -> f v
  | _ -> ()

let has_undo v =
  v.index <> -1

let has_redo v =
  v.index < v.stack##.length - 1

let set_callback v cb =
  v.callback <- Some cb

