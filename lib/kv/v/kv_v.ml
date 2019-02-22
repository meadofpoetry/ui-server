module type RO_V = sig
  type t
  val create : ?default:t -> Kv.RO.t -> Kv.RO.key list -> t option Lwt.t
end

module RO (S : sig
             type t
             val of_string : string -> t
           end) : RO_V with type t := S.t
  = struct
  let create ?default kv keys =
    let open Lwt.Infix in
    Kv.RO.read_opt kv keys >>= function
    | None -> Lwt.return default
    | Some v ->
       try Lwt.return_some @@ S.of_string v
       with _ -> Lwt.return_none
end

module type RW_V = sig
  type t
  val create : ?default:t
               -> Kv.RW.t
               -> Kv.RW.key list
               -> (< get : t Lwt.t; set : t -> unit Lwt.t; s : t React.S.t >, string)
                    result Lwt.t
                    (* TODO Error reporting *)
end

module RW (S : sig
             type t
             val equal : t -> t -> bool
             val of_string : string -> t
             val to_string : t -> string
           end) : RW_V with type t := S.t
  = struct

  let map_exn f = function
    | None as v -> v
    | Some v ->
       try Some (f v)
       with _ -> None

  let watch kv keys v =
    let open Lwt.Infix in
    let s, push = React.S.create ~eq:S.equal v in
    Kv.RW.watch kv keys (fun _ nv ->
        try Lwt.return @@ push @@ S.of_string nv
        with _ -> Lwt.return_unit)
    >>= fun () -> Lwt.return s

  let construct kv keys v =
    let open Lwt.Infix in
    watch kv keys v >>= fun s ->
    Lwt.return_ok (object
      method get = Lwt.return @@ React.S.value s
      method set x = Kv.RW.write kv keys (S.to_string x)
                     >>= fun _ -> Lwt.return_unit
      method s = s
    end)

  (* TODO better interface *)
  let create ?default kv keys =
    let open Lwt.Infix in
    Kv.RW.read_opt kv keys >>= fun v ->
    match map_exn S.of_string v, default with
    | None, None -> Lwt.return_error "No data or default value"
    | Some v, _ | None, Some v ->
       construct kv keys v
    
end
