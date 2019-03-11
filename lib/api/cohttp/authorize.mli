type error = [`Need_auth | `Wrong_password]

val auth : (name:string -> pass:string -> ('id, [> error ] as 'b) Lwt_result.t)
           -> Cohttp.Header.t
           -> ('id, 'b) Lwt_result.t
