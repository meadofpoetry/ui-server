

val home_page : ?headers:Cohttp.Header.t -> unit -> Interaction.response

val login_page : ?headers:Cohttp.Header.t -> unit -> Interaction.response

val not_found : ?uri:Netlib.Uri.t -> unit -> Interaction.response

val redirect_auth : (name:string -> pass:string -> ('a, [> Authorize.error]) result Lwt.t)
                    -> Cohttp.Header.t
                    -> ('a -> Interaction.response)
                    -> Interaction.response

val redirect_if : bool
                  -> (unit -> Interaction.response)
                  -> Interaction.response
