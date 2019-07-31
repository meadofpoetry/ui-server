module DN = struct

  let of_k_v (type a) :
    a X509.Distinguished_name.k
    -> a
    -> Server_types.Distinguished_name.k * string =
    fun k v -> match k with
      | CN -> CN, v
      | Serialnumber -> Serialnumber, v
      | C -> C, v
      | L -> L, v
      | SP -> SP, v
      | O -> O, v
      | OU -> OU, v
      | T -> T, v
      | DNQ -> DNQ, v
      | Mail -> Mail, v
      | DC -> DC, v
      | Given_name -> Given_name, v
      | Surname -> Surname, v
      | Initials -> Initials, v
      | Pseudonym -> Pseudonym, v
      | Generation -> Generation, v
      | Other oid -> Other (Format.asprintf "%a" Asn.OID.pp oid), v

  let of_distinguished_name (t : X509.Distinguished_name.t) =
    List.map (function X509.Distinguished_name.B (k, v) -> of_k_v k v)
    @@ X509.Distinguished_name.bindings t

end

let of_x509 (file : string) =
  let open X509 in
  match Certificate.decode_pem @@ Cstruct.of_string file with
  | Error `Msg e -> Error e
  | Ok x ->
    let pk = Certificate.public_key x in
    Ok { Server_types.
         serial = Cstruct.of_string @@ Z.to_bits @@ Certificate.serial x
       ; issuer = DN.of_distinguished_name @@ Certificate.issuer x
       ; validity = Certificate.validity x
       ; subject = DN.of_distinguished_name @@ Certificate.subject x
       ; public_key =
           { typ = `RSA
           ; fingerprint =
               [`SHA1, Public_key.fingerprint ~hash:`SHA1 pk]
           }
       ; fingerprints =
           [ `SHA256, Certificate.fingerprint `SHA256 x
           ; `SHA1, Certificate.fingerprint `SHA1 x
           ]
       }
