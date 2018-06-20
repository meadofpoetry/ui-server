open Containers
open Storage.Database
open Board_types
open Lwt.Infix

module Model = struct
  let name = "qos_niit"

  let init_state = Exec (Caqti_request.exec Types.unit "SELECT 1")

  let tables = [ "state", init_state, None
                                        (*  ; "ts_errors", init_, None *)
               ]
           
end

             

module Conn = Storage.Database.Make(Model)

type t = Conn.t
