module DN = struct
  let of_attribute :
         X509.Distinguished_name.attribute
      -> Server_types.Distinguished_name.attribute * string = function
    | CN v -> CN, v
    | Serialnumber v -> Serialnumber, v
    | C v -> C, v
    | L v -> L, v
    | ST v -> ST, v
    | O v -> O, v
    | OU v -> OU, v
    | T v -> T, v
    | DNQ v -> DNQ, v
    | Mail v -> Mail, v
    | DC v -> DC, v
    | Given_name v -> Given_name, v
    | Surname v -> Surname, v
    | Initials v -> Initials, v
    | Pseudonym v -> Pseudonym, v
    | Generation v -> Generation, v
    | Street v -> Street, v
    | Userid v -> Userid, v
    | Other (oid, v) -> Other (Format.asprintf "%a" Asn.OID.pp oid), v

  let of_distinguished_name (t : X509.Distinguished_name.t) =
    List.map of_attribute
    @@ List.flatten
    @@ List.map X509.Distinguished_name.Relative_distinguished_name.elements t
end

let of_x509 (file : string) =
  let open X509 in
  match Certificate.decode_pem @@ Cstruct.of_string file with
  | Error (`Msg e) -> Error e
  | Ok x ->
      let pk = Certificate.public_key x in
      Ok
        { Server_types.serial = Cstruct.of_string @@ Z.to_bits @@ Certificate.serial x
        ; issuer = DN.of_distinguished_name @@ Certificate.issuer x
        ; validity = Certificate.validity x
        ; subject = DN.of_distinguished_name @@ Certificate.subject x
        ; public_key =
            {typ = `RSA; fingerprint = [`SHA1, Public_key.fingerprint ~hash:`SHA1 pk]}
        ; fingerprints =
            [ `SHA256, Certificate.fingerprint `SHA256 x
            ; `SHA1, Certificate.fingerprint `SHA1 x ] }
