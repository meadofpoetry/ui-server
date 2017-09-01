module Make(MD : Board_meta.MSG_DESC)
       : (Board_meta.BOARD_API) = struct

  [@@@ocaml.warning "-37"]

  type mode =
    | T2
    | T 
    | C

  type bw =
    | Bw8
    | Bw7
    | Bw6


  type dvbc_qam =
    | Qam32
    | Qam64
    | Qam128
    | Qam256 

  type rsp_devinfo =
    { serial   : int
    ; hw_ver   : int
    ; fpga_ver : int
    ; soft_ver : int
    ; asi      : bool
    ; modules  : int list
    } [@@deriving to_yojson]

  type settings =
    { mode : mode
    ; bw   : bw
    ; dvbc_qam : dvbc_qam
    ; freq     : int32
    ; plp_id   : int
    } 

  type rsp_settings =
    { settings   : settings
    ; hw_present : bool
    ; lock       : bool
    } 

  type rsp_measure =
    { lock    : bool
    ; power   : float option
    ; mer     : float option
    ; ber     : float option
    ; freq    : int32 option
    ; bitrate : int32 option
    } 

  type rsp_plp_list =
    { lock    : bool
    ; plps    : int list
    }

  type rsp_plp_set =
    { lock    : bool
    ; plp     : int
    }

  type resp  = Ack
             | Devinfo     of rsp_devinfo
             | Settings    of (int * rsp_settings)
             | Measure     of (int * rsp_measure)
             | Plps        of (int * rsp_plp_list)
             | Plp_setting of (int * rsp_plp_set)

  type req   = Devinfo     of bool
             | Settings    of int * settings
             | Measure     of int
             | Plps        of int
             | Plp_setting of int * int

  let handle send _ id meth args _ _ _ =
    let open Lwt.Infix in
    let open Redirect in
    let redirect_if_guest = redirect_if (User.eq id `Guest) in
    match meth, args with
    | `POST, ["settings"] -> redirect_if_guest not_found
    | `POST, ["plp"]      -> redirect_if_guest not_found
    | `GET,  ["devinfo"]  -> (send (Devinfo false))
                             >>= (function
                                  | ((Devinfo x):resp) -> Interaction.respond_js (rsp_devinfo_to_yojson x) ()
                                  | _ -> not_found ())
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
