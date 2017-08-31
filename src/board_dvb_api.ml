module Make(MD : Board_meta.MSG_DESC)
       : (Board_meta.BOARD_API with type resp := MD.resp
                                and type req := MD.req) = struct

  let handlers _ _ _ _ =
    [ (module struct
         let domain = "1"
         let handle = (fun _ _ _ _ _ _ -> Redirect.not_found ())
       end : Api_handler.HANDLER) ]
end
