module DN = struct
  let attribute_to_kv : X509.Distinguished_name.attribute -> string * string =
    function
    | CN v -> ("Common Name (CN)", v)
    | Serialnumber v -> ("Serial Number", v)
    | C v -> ("Country Name (C)", v)
    | L v -> ("Locality Name (L)", v)
    | ST v -> ("State or Province (SP)", v)
    | O v -> ("Organization (O)", v)
    | OU v -> ("Organization Unit (ON)", v)
    | T v -> ("Title (T)", v)
    | DNQ v -> ("Distinguished Name Qualifier (DNQ)", v)
    | Mail v -> ("Email address", v)
    | DC v -> ("Domain Component (DC)", v)
    | Given_name v -> ("Given Name", v)
    | Surname v -> ("Surname", v)
    | Initials v -> ("Initials", v)
    | Pseudonym v -> ("Pseudonym", v)
    | Generation v -> ("Generation", v)
    | Street v -> ("Street", v)
    | Userid v -> ("User ID", v)
    | Other (oid, v) -> (Format.asprintf "OID: %a" Asn.OID.pp oid, v)
end

let of_x509 (file : string) =
  let open X509 in
  match Certificate.decode_pem @@ Cstruct.of_string file with
  | Error (`Msg e) -> Error e
  | Ok x ->
      let pk = Certificate.public_key x in
      Ok
        {
          Server_types.serial =
            Cstruct.of_string @@ Z.to_bits @@ Certificate.serial x;
          issuer =
            List.map DN.attribute_to_kv
            @@ List.flatten
            @@ List.map Distinguished_name.Relative_distinguished_name.elements
            @@ Certificate.issuer x;
          validity = Certificate.validity x;
          subject =
            List.map DN.attribute_to_kv
            @@ List.flatten
            @@ List.map Distinguished_name.Relative_distinguished_name.elements
            @@ Certificate.subject x;
          public_key =
            {
              typ = `RSA;
              fingerprint = [ (`SHA1, Public_key.fingerprint ~hash:`SHA1 pk) ];
            };
          fingerprints =
            [
              (`SHA256, Certificate.fingerprint `SHA256 x);
              (`SHA1, Certificate.fingerprint `SHA1 x);
            ];
        }
