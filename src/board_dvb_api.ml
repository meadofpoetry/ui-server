module Make(MD : Board_meta.MSG_DESC)
       : (Board_meta.BOARD_API with type resp := MD.resp
                                and type 'a req := 'a MD.req) = struct

end
