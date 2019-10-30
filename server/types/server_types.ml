module Show_cstruct = struct
  type t = Cstruct.t

  let of_yojson = function
    | `String s -> Ok (Cstruct.of_string @@ Base64.decode_exn s)
    | _ -> Error "cstruct_of_yojson: invalid json"
  let to_yojson (x : Cstruct.t) : Yojson.Safe.t =
    `String (Base64.encode_exn @@ Cstruct.to_string x)
end

type settings =
  { https_enabled : bool
  ; tls_cert : (string * certificate) option
  ; tls_key : string option
  } [@@deriving yojson]
and certificate =
  { serial : Show_cstruct.t
  ; issuer : (string * string) list
  ; validity : Time.t * Time.t
  ; subject : (string * string) list
  ; public_key : public_key
  ; fingerprints : (hash * Show_cstruct.t) list
  }
and public_key =
  { typ : [`RSA]
  ; fingerprint : (hash * Show_cstruct.t) list
  }
and hash =
  [ `SHA1
  | `SHA256
  ]
