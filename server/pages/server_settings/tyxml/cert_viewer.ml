open Components_tyxml

module CSS = struct
  let root = "certificate-viewer"
  let groups = root ^ "-groups"
  let group_title = BEM.add_element groups "title"
  let row = BEM.add_element groups "row"
  let attribute = BEM.add_element groups "attribute"
  let value = BEM.add_element groups "value"
  let serial = BEM.add_element groups "serial"
end

module Make(Xml : Xml_sigs.NoWrap)
    (Svg : Svg_sigs.NoWrap with module Xml := Xml)
    (Html : Html_sigs.NoWrap with module Xml := Xml
                              and module Svg := Svg) = struct
  open Html

  let make_group_record ?(classes = []) ?(attrs = []) ~attribute ~value () =
    let classes = CSS.row :: classes in
    div ~a:([a_class classes] @ attrs)
      [ div ~a:[a_class [CSS.attribute]] [txt attribute]
      ; div ~a:[a_class [CSS.value]] [txt value]
      ]

  let make_group_title ?(classes = []) ?(attrs = []) title =
    let classes = CSS.row :: CSS.group_title :: classes in
    div ~a:([a_class classes] @ attrs) [h3 [txt title]]

  let make_groups ?(classes = []) ?(attrs = []) content =
    let classes = CSS.groups :: classes in
    div ~a:([a_class classes] @ attrs) content

  let serial ({ serial; _ } : Server_types.certificate) =
    let sprintf = Printf.sprintf in
    let iter =
      Cstruct.iter
        (fun _ -> Some 1)
        (fun x -> sprintf "%02X" @@ Cstruct.get_uint8 x 0)
        serial in
    let value = Cstruct.fold (function
        | "" -> fun x -> x
        | acc -> sprintf "%s %s" acc) iter "" in
    [ make_group_title "Serial number"
    ; div ~a:[a_class [CSS.row]] [div ~a:[a_class [CSS.serial]] [txt value]]
    ]

  let issuer ({ issuer; _ } : Server_types.certificate) =
    let rows =
      List.map (fun (k, v) ->
          make_group_record ~attribute:k ~value:v ())
        issuer in
    make_group_title "Issuer" :: rows

  let subject ({ subject; _ } : Server_types.certificate) =
    let rows =
      List.map (fun (k, v) ->
          make_group_record ~attribute:k ~value:v ())
        subject in
    make_group_title "Subject" :: rows

  let fingerprints ({ fingerprints; _ } : Server_types.certificate) =
    let rows =
      List.map (fun (hash, value) ->
          let attribute = match hash with
            | `SHA1 -> "SHA1 Fingerprint"
            | `SHA256 -> "SHA256 Fingerprint" in
          let sprintf = Printf.sprintf in
          let iter =
            Cstruct.iter
              (fun _ -> Some 1)
              (fun x -> sprintf "%02X" @@ Cstruct.get_uint8 x 0)
              value in
          let value = Cstruct.fold (function
              | "" -> fun x -> x
              | acc -> sprintf "%s %s" acc) iter "" in
          make_group_record ~attribute ~value ())
        fingerprints in
    make_group_title "Fingerprints" :: rows

  let validity ?tz_offset_s ({ validity; _ } : Server_types.certificate) =
    let rows =
      [ make_group_record
          ~attribute:"Issued On"
          ~value:(Time.to_human_string ?tz_offset_s (fst validity))
          ()
      ; make_group_record
          ~attribute:"Expires On"
          ~value:(Time.to_human_string ?tz_offset_s (snd validity))
          ()
      ] in
    make_group_title "Validity" :: rows

  let of_certificate ?tz_offset_s ?(classes = []) ?(attrs = [])
      (cert : Server_types.certificate) =
    let classes = CSS.root :: classes in
    div ~a:([a_class classes] @ attrs)
      [ make_groups (serial cert)
      ; make_groups (issuer cert
                     @ subject cert
                     @ validity ?tz_offset_s cert
                     @ fingerprints cert)
      ]

end
