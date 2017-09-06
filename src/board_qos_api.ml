module Make(P : Board_meta.PROTOCOL)
       : (Board_meta.BOARD_API with type event := P.event
                                and type response := P.response
                                and type 'a request := 'a P.request) = struct

  let handle _ _ _ meth args _ _ _ =
    let open Lwt.Infix in
    let open Redirect in
    match meth, args with
    | _ -> not_found ()

  let handlers id send _ _ _ =
    [ (module struct
         let domain = Common.Hardware.get_api_path id
         let handle = handle send ()
       end : Api_handler.HANDLER) ]

end
