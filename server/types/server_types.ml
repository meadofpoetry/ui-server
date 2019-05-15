
type settings = { https_enabled : bool
                ; tls_cert : string option
                ; tls_key : string option
                } [@@deriving yojson]
