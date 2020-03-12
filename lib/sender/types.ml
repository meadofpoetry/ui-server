type identity = ..

class virtual sender =
  object
    method virtual id : identity

    method virtual depends : identity option

    method virtual path : string

    method virtual to_send : bool

    method virtual check : Cohttp.Code.status_code -> unit

    method virtual get_data : Caqti_lwt.connection -> Yojson.Safe.t option Lwt.t

    method virtual cleanup : Caqti_lwt.connection -> unit Lwt.t
  end
