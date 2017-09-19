open Tyxml
   
type color_scheme = Primary
                  | Accent

let to_string component =
  Format.asprintf "%a" (Html.pp_elt ()) component

let add_common_attrs ?id ?style l =
  l
  |> (fun l -> match id with
               | Some x -> l @ [Html.a_id x]
               | None   -> l)
  |> (fun l -> match style with
               | Some x -> l @ [Html.a_style x]
               | None   -> l)
  
module Button = struct
  let base_class = "mdc-button"

  let create ?(classes=[])
        ?id
        ?style
        ?(disabled=false)
        ?color_scheme
        ?(raised=false)
        ?(ripple=false)
        ?(dense=false)
        ?(compact=false)
        label
        () =
    Html.button ~a:([Html.a_class (base_class :: classes
                                   |> (fun l -> match color_scheme with
                                                | None   -> l
                                                | Some x -> match x with
                                                            | Primary -> (base_class ^ "--primary") :: l
                                                            | Accent  -> (base_class ^ "--accent") :: l)
                                   |> (fun l -> if raised  then (base_class ^ "--raised") :: l  else l)
                                   |> (fun l -> if dense   then (base_class ^ "--dense") :: l   else l)
                                   |> (fun l -> if compact then (base_class ^ "--compact") :: l else l))]
                    |> add_common_attrs ?id ?style
                    |> (fun l -> if disabled then l @ [Html.a_disabled ()]                           else l)
                    |> (fun l -> if ripple   then l @ [Html.a_user_data "mdc-auto-init" "MDCRipple"] else l))
      [Html.pcdata label]
end

module Card = struct

  let base_class = "mdc-card"

  (* Media section *)

  let create_media ?(classes=[])
        ?id
        ?style
        children
        () =
    Html.section ~a:([Html.a_class ((base_class ^ "__media") :: classes)]
                     |> add_common_attrs ?id ?style)
      children

  (* Actions section *)

  let create_actions ?(classes=[])
        ?id
        ?style
        children
        () =
    Html.section ~a:([Html.a_class ((base_class ^ "__actions") :: classes)]
                     |> add_common_attrs ?id ?style)
      children

  (* Primary section *)

  let create_title ?(classes=[])
        ?id
        ?style
        ?(large=false)
        title
        () =
    let class' = base_class ^ "__title" in
    Html.h1 ~a:([Html.a_class (class' :: classes
                               |> (fun l -> if large then (class' ^ "--large") :: l else l))]
                |> add_common_attrs ?id ?style)
      [Html.pcdata title]

  let create_subtitle ?(classes=[])
        ?id
        ?style
        subtitle
        () =
    Html.h2 ~a:([Html.a_class ((base_class ^ "__subtitle") :: classes)]
                |> add_common_attrs ?id ?style)
      [Html.pcdata subtitle]

  let create_primary ?(classes=[])
        ?id
        ?style
        children
        () =
    Html.section ~a:([Html.a_class ((base_class ^ "__primary") :: classes)]
                     |> add_common_attrs ?id ?style)
      children

  (* Supporting text section *)

  let create_supporting_text ?(classes=[])
        ?id
        ?style
        children
        () =
    Html.section ~a:([Html.a_class ((base_class ^ "__supporting-text") :: classes)]
                     |> add_common_attrs ?id ?style)
      children

  (* Card *)

  let create ?(sections=[])
        ?id
        ?style
        ?(classes=[])
        () =
    Html.div ~a:([Html.a_class ("mdc-card" :: classes)]
                 |> add_common_attrs ?id ?style)
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

  type tab_content = Text of string
                   | Icon of (string * string option)
                   | Text_and_icon of (string * string)

  let base_class = "mdc-tab-bar"

  let tab_class = "mdc-tab"

  let create_tab ?(classes=[])
        ?id
        ?style
        ?(active=false)
        ?href
        tab_content
        () =
    Html.a ~a:([Html.a_class (tab_class :: classes
                              |> (fun l -> if active then (tab_class ^ "--active") :: l else l))]
               |> add_common_attrs ?id ?style
               |> (fun l -> match href with
                            | Some x -> l @ [Html.a_href x]
                            | None   -> l))
      (match tab_content with
       | Text s -> [Html.pcdata s]
       | Icon (i,fallback) -> [Html.i ~a:([Html.a_class ["material-icons"; (tab_class ^ "__icon")]]
                                          |> (fun l -> match fallback with
                                                       | Some x -> l @ [Html.a_aria "label" [x]]
                                                       | None   -> l))
                                 [Html.pcdata i]]
       | Text_and_icon (s,i) -> [ Html.i ~a:([ Html.a_class ["material-icons"; (tab_class ^ "__icon")]
                                             ; Html.a_aria "hidden" ["true"]])
                                    [Html.pcdata i]
                                ; Html.span ~a:[Html.a_class [(tab_class ^ "__icon-text")]]
                                    [Html.pcdata s]])

  let create_indicator ?(classes=[])
        ?id
        ?style
        () =
    Html.span ~a:([Html.a_class ((base_class ^ "__indicator") :: classes)]
                  |> add_common_attrs ?id ?style) []

  let create ?(classes=[])
        ?id
        ?style
        ?(with_indicator=true)
        ?(accent_indicator=false)
        tabs_type
        children
        () =
    Html.nav ~a:([ Html.a_class (base_class :: classes
                                 |> (fun l -> match tabs_type with
                                              | `Text          -> l
                                              | `Icon          -> (base_class ^ "--icon-tab-bar") :: l
                                              | `Text_and_icon -> (base_class ^ "--icons-with-text") :: l)
                                 |> (fun l -> if accent_indicator
                                              then (base_class ^ "--indicator-accent") :: l
                                              else l))
                 ; Html.a_user_data "mdc-auto-init" "MDCTabBar"]
                 |> add_common_attrs ?id ?style)
      (children |> (fun x -> if with_indicator then (x @ [create_indicator ()]) else x))

end

module Text_field = struct

  let base_class = "mdc-textfield"

  let create ?(classes=[])
        ?id
        ?style
        ?(password=false)
        ?(disabled=false)
        label
        () =
    Html.label ~a:([ Html.a_class (base_class :: classes)
                   ; Html.a_user_data "mdc-auto-init" "MDCTextfield" ]
                   |> add_common_attrs ?id ?style)
      [ Html.input ~a:([ Html.a_class [base_class ^ "__input"]
                       ; Html.a_input_type (if password then `Password else `Text) ]
                       |> (fun l -> if disabled then l @ [Html.a_disabled ()] else l))
          ()
      ; Html.span ~a:[Html.a_class [base_class ^ "__label"]]
          [Html.pcdata label]]

end

module Toolbar = struct

end
