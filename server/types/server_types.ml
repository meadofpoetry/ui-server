module Distinguished_name = struct

  type k =
    | CN
    | Serialnumber
    | C
    | L
    | SP
    | O
    | OU
    | T
    | DNQ
    | Mail
    | DC
    | Given_name
    | Surname
    | Initials
    | Pseudonym
    | Generation
    | Other of string [@@deriving yojson]

  let k_to_string = function
    | CN -> "Common Name (CN)"
    | Serialnumber -> "Serial Number"
    | C -> "Country Name (C)"
    | L -> "Locality Name (L)"
    | SP -> "State or Province (SP)"
    | O -> "Organization (O)"
    | OU -> "Organization Unit (ON)"
    | T -> "Title (T)"
    | DNQ -> "Distinguished Name Qualifier (DNQ)"
    | Mail -> "Email address"
    | DC -> "Domain Component (DC)"
    | Given_name -> "Given Name"
    | Surname -> "Surname"
    | Initials -> "Initials"
    | Pseudonym -> "Pseudonym"
    | Generation -> "Generation"
    | Other oid -> oid

  type t = (k * string) list [@@deriving yojson]

end

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
  ; issuer : Distinguished_name.t
  ; validity : Time.t * Time.t
  ; subject : Distinguished_name.t
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
