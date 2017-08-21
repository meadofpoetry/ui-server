open Tyxml.Html

type color_scheme = Primary
                  | Accent

let to_string component =
  Format.asprintf "%a" (pp_elt ()) component

module Button = struct
  let base_class = "mdc-button"

  let create ?(classes=[])
             ?color_scheme
             ?(raised=false)
             ?(ripple=false)
             ?(dense=false)
             ?(compact=false)
             ?(disabled=false)
             label
             () =
    button ~a:([a_class (base_class :: classes
                         |> List.append (match color_scheme with
                                         | None   -> []
                                         | Some x -> match x with
                                                     | Primary -> [base_class ^ "--primary"]
                                                     | Accent  -> [base_class ^ "--accent"])
                         |> List.append (if raised  then [base_class ^ "--raised"]  else [])
                         |> List.append (if dense   then [base_class ^ "--dense"]   else [])
                         |> List.append (if compact then [base_class ^ "--compact"] else []))]
               |> List.append (if disabled then [a_disabled ()]                           else [])
               |> List.append (if ripple   then [a_user_data "mdc-auto-init" "MDCRipple"] else []))
           [pcdata label]
end

module Card = struct

  let base_class = "mdc-card"

  (* Media section *)

  let create_media ?(classes=[])
                   children
                   () =
    let class' = base_class ^ "__media" in
    section ~a:[a_class (class' :: classes)]
            children

  (* Actions section *)

  let create_actions ?(classes=[])
                     children
                     () =
    let class' = base_class ^ "__actions" in
    section ~a:[a_class (class' :: classes)]
            children

  (* Primary section *)

  let create_title ?(classes=[])
                   ?(large=false)
                   title
                   () =
    let class' = base_class ^ "__title" in
    h1 ~a:[a_class (List.append (class' :: classes)
                                (if large then [class' ^ "--large"] else []))]
       [pcdata title]

  let create_subtitle ?(classes=[])
                      subtitle
                      () =
    let class' = base_class ^ "__subtitle" in
    h2 ~a:[a_class (class' :: classes)]
       [pcdata subtitle]

  let create_primary ?(classes=[])
                     children
                     () =
    section ~a:[a_class ((base_class ^ "__primary") :: classes)]
            children

  (* Supporting text section *)

  let create_supporting_text ?(classes=[])
                             children
                             () =
    let class' = base_class ^ "__supporting_text" in
    section ~a:[a_class (class' :: classes)]
            children

  (* Card *)

  let create ?(sections=[])
             ?id
             ?(classes=[])
             () =
    div ~a:([a_class ("mdc-card" :: classes)]
            |> List.append (match id with
                            | Some x -> [a_id x]
                            | None   -> []))
        sections

end

module Checkbox = struct

end

module Dialog = struct

end

module Elevation = struct

end

module Fab = struct

end

module Grid_list = struct

end

module Icon_toggle = struct

end

module Layout_grid = struct

end

module Linear_progress = struct

end

module List_ = struct

end

module Radio_button = struct

end

module Select = struct

end

module Simple_menu = struct

end

module Slider = struct

end

module Snackbar = struct

end

module Switch = struct

end

module Tabs = struct

end

module Text_field = struct

end

module Toolbar = struct

end
