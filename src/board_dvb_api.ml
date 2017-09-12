let handle _ _ _ meth args _ _ _ =
  let open Lwt.Infix in
  let open Redirect in
  (* let redirect_if_guest = redirect_if (User.eq id `Guest) in *)
  match meth, args with
  | `POST, ["reset"]    -> not_found ()
  | `POST, ["settings"] -> not_found ()
  | `POST, ["plp"]      -> not_found ()
  | _ -> not_found ()

let handlers id send _ _ _ =
  [ (module struct
       let domain = Common.Hardware.get_api_path id
       let handle = handle send ()
     end : Api_handler.HANDLER) ]

                                                           (* (match P.make_req ("settings", Some body) with *)
                                                           (* | Error e -> Interaction.respond_error e () *)
                                                           (* | Ok req  -> send req *)
                                                           (*              >>= fun resp -> *)
                                                           (*              Interaction.respond_js (P.to_yojson resp) ()) *)
