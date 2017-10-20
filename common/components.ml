open Tyxml

let to_string component =
  Format.asprintf "%a" (Html.pp_elt ()) component

                  [@@@ocaml.warning "-60"]

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
                         and module Svg := Svg) = struct
  open Html
     
  type color_scheme = Primary
                    | Accent

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

    let base_class = "mdc-checkbox"

    let create ?(classes=[]) ?style
               ?id ?input_id
               ?(disabled=false) ?(js=true) () =
      Html.div ~a:([Html.a_class (base_class :: classes
                                  |> (fun x -> if disabled then (base_class ^ "--disabled") :: x else x))]
                   |> add_common_attrs ?id ?style
                   |> (fun x -> if js then x @ [Html.a_user_data "mdc-auto-init" "MDCCheckbox"] else x))
               [ Html.input ~a:([ Html.a_input_type `Checkbox
                                ; Html.a_class [base_class ^ "__native-control"]]
                                |> add_common_attrs ?id:input_id ?style:None
                                |> fun x -> if disabled then Html.a_disabled () :: x else x) ()
               ; Html.div ~a:([Html.a_class [base_class ^ "__background"]])
                          [ Html.svg ~a:([ Svg.a_class [base_class ^ "__checkmark"]
                                         ; Svg.a_viewBox (0.0, 0.0, 24.0, 24.0)])
                                     [Svg.path ~a:([ Svg.a_class [base_class ^ "__checkmark__path"]
                                                   ; Svg.a_fill `None
                                                   ; Svg.a_stroke (`Color ("white",None))
                                                   ; Svg.a_d "M1.73,12.91 8.1,19.28 22.79,4.59"])
                                               []]]
               ; Html.div ~a:([Html.a_class [base_class ^ "__mixedmark"]])
                          []]

    let create_with_label ?classes ?(form_classes=[])
                          ?id ~input_id ?form_id
                          ?style ?form_style
                          ?disabled ?js ~label () =
      Html.div ~a:([Html.a_class ("mdc-form-field" :: form_classes)]
                   |> add_common_attrs ?id:form_id ?style:form_style)
               [ create ?classes ?id ~input_id ?style ?disabled ?js ()
               ; Html.label ~a:([Html.a_label_for input_id]) [Html.pcdata label] ]

  end

  module Dialog = struct

    let base_class = "mdc-dialog"

    let create_label ~id ~label () =
      Html.header ~a:([ Html.a_class [base_class ^ "__header"]])
                  [ Html.h2 ~a:([ Html.a_class [base_class ^ "__header__title" ]]
                                |> add_common_attrs ~id ?style:None)
                            [ Html.pcdata label]]

    let create_section ?(scrollable=false) ~id ~content () =
      let _class = base_class ^ "__body" in
      Html.section ~a:([ Html.a_class ([_class]
                                       |> fun x -> if scrollable then x @ [_class ^ "--scrollable"] else x) ]
                       |> add_common_attrs ~id ?style:None)
                   content

    let create_button ?ripple ?(action=false) ~_type ~label () =
      let _class = base_class ^ "__footer__button" in
      Button.create ~classes:([ _class
                              ; match _type with
                                | `Accept  -> _class ^ "--accept"
                                | `Decline -> _class ^ "--cancel" ]
                              |> fun x -> if action then x @ [base_class ^ "__action"] else x)
                    label
                    ?ripple
                    ()

    let create_footer ~children () =
      Html.footer ~a:([ Html.a_class [base_class ^ "__footer"]]) children

    let create ?id ?style ?(classes=[])
               ~label_id ~description_id ~content () =
      Html.aside ~a:([ Html.a_class (base_class :: classes)
                     ; Html.a_role ["alertdialog"]
                     ; Html.a_aria "labelledby" [label_id]
                     ; Html.a_aria "describedby" [description_id]
                     ; Html.a_user_data "mdc-auto-init" "MDCDialog"]
                     |> add_common_attrs ?id ?style)
                 [ Html.div ~a:([Html.a_class [base_class ^ "__surface"]]) content
                 ; Html.div ~a:([Html.a_class [base_class ^ "__backdrop"]]) []
                 ]

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

    let base_class = "mdc-switch"

    let create_label ?id ?style ?(classes=[]) ~id_for ~label () =
      Html.label ~a:([ Html.a_class ((base_class ^ "-label") :: classes)
                     ; Html.a_label_for id_for ]
                     |> add_common_attrs ?id ?style)
                 [Html.pcdata label]

    let create ?id ?input_id ?style ?(classes=[]) ?(disabled=false) () =
      Html.div ~a:([Html.a_class (base_class :: classes
                                  |> fun x -> if disabled then x @ [base_class ^ "--disabled"] else x)]
                   |> add_common_attrs ?id ?style)
               [ Html.input ~a:([ Html.a_input_type `Checkbox
                                ; Html.a_class [base_class ^ "__native-control"]]
                                |> fun x -> if disabled then (Html.a_disabled ()) :: x else x
                                |> add_common_attrs ?id:input_id ?style:None) ()
               ; Html.div ~a:([ Html.a_class [base_class ^ "__background"]])
                          [ Html.div ~a:([ Html.a_class [base_class ^ "__knob"]]) []]
               ]

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

end
