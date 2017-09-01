module Make(P : Board_meta.PROTOCOL)
       : (Board_meta.BOARD_API with type resp := P.resp
                                and type req := P.req) = struct

  let handle send _ id meth args _ _ _ =
    let open Lwt.Infix in
    let open Redirect in
    let redirect_if_guest = redirect_if (User.eq id `Guest) in
    match meth, args with
    | `POST, ["settings"] -> redirect_if_guest not_found
    | `POST, ["plp"]      -> redirect_if_guest not_found
    | `GET,  ["devinfo"]  -> (match P.make_req ("devinfo", None) with
                             | Error e -> Interaction.respond_error e ()
                             | Ok req  -> send req
                                          >>= fun resp ->
                                          Interaction.respond_js (P.to_yojson resp) ())
    | `GET,  ["params"]   -> not_found ()
    | `GET,  ["meas"]     -> not_found ()
    | `GET,  ["plps"]     -> not_found ()
    | _ -> not_found ()

  let handlers id send _ _ =
    [ (module struct
         let domain = Common.Hardware.get_api_path id
         let handle = handle send ()
       end : Api_handler.HANDLER) ]

end
