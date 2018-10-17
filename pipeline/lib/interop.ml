type 'a msg = { name : string
              ; data : 'a
              } [@@deriving yojson]

type 'a cont_body = { _method : string [@key "method"]
                    ; body : 'a
                    } [@@deriving yojson]

type cont_met = { _method : string [@key "method"]
                } [@@deriving yojson]

type 'a channel = { get : unit -> ('a, string) result Lwt.t
                  ; set : 'a -> (unit, string) result Lwt.t
                  }
              
type 'a request = (unit -> ('a, string) result Lwt.t)
