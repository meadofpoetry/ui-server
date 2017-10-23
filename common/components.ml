open Tyxml

       [@@@ocaml.warning "-60"]

let to_string component = Format.asprintf "%a" (Html.pp_elt ()) component

module Make
         (Xml : Xml_sigs.NoWrap)
         (Svg : Svg_sigs.NoWrap with module Xml := Xml)
         (Html : Html_sigs.NoWrap
          with module Xml := Xml
                         and module Svg := Svg) = struct
  open Html

  let add_common_attrs ?id ?style ?(attrs=[]) l =
    l
    |> (fun l -> match id with
                 | Some x -> l @ [Html.a_id x]
                 | None   -> l)
    |> (fun l -> match style with
                 | Some x -> l @ [Html.a_style x]
                 | None   -> l)
    |> (fun l -> l @ attrs)

  let toelt = Html.toelt

  module Button = struct

    let base_class = "mdc-button"

    let create ?(classes=[]) ?id ?style
               ?(disabled=false) ?color_scheme ?(raised=false) ?(ripple=false)
               ?(dense=false) ?(compact=false) ?label ?onclick ?attrs () =
      button ~a:([ a_class (base_class :: classes
                            |> (fun l -> match color_scheme with
                                         | None   -> l
                                         | Some x -> match x with
                                                     | `Primary -> (base_class ^ "--primary") :: l
                                                     | `Accent  -> (base_class ^ "--accent") :: l)
                            |> (fun l -> if raised  then (base_class ^ "--raised") :: l  else l)
                            |> (fun l -> if dense   then (base_class ^ "--dense") :: l   else l)
                            |> (fun l -> if compact then (base_class ^ "--compact") :: l else l)) ]
                 |> add_common_attrs ?id ?style ?attrs
                 |> (fun l -> match onclick with
                              | Some x -> l @ [a_onclick x]
                              | None   -> l)
                 |> (fun l -> if disabled then l @ [a_disabled ()]                           else l)
                 |> (fun l -> if ripple   then l @ [a_user_data "mdc-auto-init" "MDCRipple"] else l))
             (match label with
              | Some label -> [pcdata label]
              | None       -> [])
  end

  module Card = struct

    let base_class = "mdc-card"

    (* Media section *)

    let create_media ?(classes=[]) ?id ?style ?attrs ~children () =
      section ~a:([a_class ((base_class ^ "__media") :: classes)]
                  |> add_common_attrs ?id ?style ?attrs)
              children

    (* Actions section *)

    let create_actions ?(classes=[]) ?id ?style ?attrs ~children () =
      section ~a:([a_class ((base_class ^ "__actions") :: classes)]
                  |> add_common_attrs ?id ?style ?attrs)
              children

    (* Primary section *)

    let create_title ?(classes=[]) ?id ?style ?attrs ?(large=false) ~title () =
      let class' = base_class ^ "__title" in
      h1 ~a:([a_class (class' :: classes
                       |> (fun l -> if large then (class' ^ "--large") :: l else l))]
             |> add_common_attrs ?id ?style ?attrs)
         [pcdata title]

    let create_subtitle ?(classes=[]) ?id ?style ?attrs ~subtitle () =
      h2 ~a:([a_class ((base_class ^ "__subtitle") :: classes)]
             |> add_common_attrs ?id ?style ?attrs)
         [pcdata subtitle]

    let create_primary ?(classes=[]) ?id ?style ?attrs ~children () =
      section ~a:([a_class ((base_class ^ "__primary") :: classes)]
                  |> add_common_attrs ?id ?style ?attrs)
              children

    (* Supporting text section *)

    let create_supporting_text ?(classes=[]) ?id ?style ?attrs ~children () =
      section ~a:([a_class ((base_class ^ "__supporting-text") :: classes)]
                  |> add_common_attrs ?id ?style ?attrs)
              children

    (* Card *)

    let create ?(sections=[]) ?id ?style ?(classes=[]) ?attrs () =
      div ~a:([a_class ("mdc-card" :: classes)]
              |> add_common_attrs ?id ?style ?attrs)
          sections

  end

  module Checkbox = struct

    let base_class = "mdc-checkbox"

    let create ?(classes=[]) ?style ?id ?input_id ?(disabled=false) ?(js=true) ?attrs () =
      div ~a:([a_class (base_class :: classes
                        |> (fun x -> if disabled then (base_class ^ "--disabled") :: x else x))]
              |> add_common_attrs ?id ?style ?attrs
              |> (fun x -> if js then x @ [a_user_data "mdc-auto-init" "MDCCheckbox"] else x))
          [ input ~a:([ a_input_type `Checkbox
                      ; a_class [base_class ^ "__native-control"]]
                      |> add_common_attrs ?id:input_id ?style:None
                      |> fun x -> if disabled then a_disabled () :: x else x) ()
          ; div ~a:([a_class [base_class ^ "__background"]])
                [ svg ~a:([ Svg.a_class [base_class ^ "__checkmark"]
                          ; Svg.a_viewBox (0.0, 0.0, 24.0, 24.0)])
                      [Svg.path ~a:([ Svg.a_class [base_class ^ "__checkmark__path"]
                                    ; Svg.a_fill `None
                                    ; Svg.a_stroke (`Color ("white",None))
                                    ; Svg.a_d "M1.73,12.91 8.1,19.28 22.79,4.59"])
                                []]]
          ; div ~a:([a_class [base_class ^ "__mixedmark"]])
                []]

    let create_with_label ?classes ?(form_classes=[]) ?attrs
                          ?id ~input_id ?form_id
                          ?style ?form_style
                          ?disabled ?js ~label () =
      div ~a:([a_class ("mdc-form-field" :: form_classes)]
              |> add_common_attrs ?id:form_id ?style:form_style)
          [ create ?classes ?id ~input_id ?style ?attrs ?disabled ?js ()
          ; Html.label ~a:([a_label_for input_id]) [pcdata label] ]

  end

  module Dialog = struct

    let base_class = "mdc-dialog"

    let create_label ~id ?style ?(classes=[]) ?attrs ~label () =
      header ~a:([ a_class [base_class ^ "__header"]])
             [ h2 ~a:([ a_class ((base_class ^ "__header__title") :: classes) ]
                      |> add_common_attrs ~id ?style ?attrs)
                  [ pcdata label]]

    let create_section ?(scrollable=false) ~id ?style ?(classes=[]) ?attrs ~children () =
      let _class = base_class ^ "__body" in
      section ~a:([ a_class ([_class]
                             |> (fun x -> if scrollable then x @ [_class ^ "--scrollable"] else x)
                             |> (fun x -> x @ classes)) ]
                  |> add_common_attrs ~id ?style ?attrs)
              children

    let create_button ?id ?style ?(classes=[]) ?attrs ?ripple ?(action=false) ~_type ~label () =
      let _class = base_class ^ "__footer__button" in
      Button.create ~classes:([ _class
                              ; match _type with
                                | `Accept  -> _class ^ "--accept"
                                | `Decline -> _class ^ "--cancel" ]
                              |> (fun x -> if action then x @ [base_class ^ "__action"] else x)
                              |> (fun x -> x @ classes))
                    ?id ?style ?attrs ?ripple ~label ()

    let create_footer ?id ?style ?(classes=[]) ?attrs ~children () =
      footer ~a:([ a_class ((base_class ^ "__footer") :: classes)]
                 |> add_common_attrs ?id ?style ?attrs) children

    let create ?id ?style ?(classes=[]) ?attrs
               ~label_id ~description_id ~content () =
      aside ~a:([ a_class (base_class :: classes)
                ; a_role ["alertdialog"]
                ; a_aria "labelledby" [label_id]
                ; a_aria "describedby" [description_id]
                ; a_user_data "mdc-auto-init" "MDCDialog"]
                |> add_common_attrs ?id ?style ?attrs)
            [ div ~a:([a_class [base_class ^ "__surface"]]) content
            ; div ~a:([a_class [base_class ^ "__backdrop"]]) []
            ]

  end

  module Fab = struct

    let base_class = "mdc-fab"

    let create ?id ?style ?(classes=[]) ?attrs ?(mini=false) ?(plain=false) ?(ripple=false) ?label ~icon () =
      button ~a:([a_class (base_class :: "material-icons" :: classes
                           |> (fun x -> if mini then x @ [base_class ^ "--mini"] else x)
                           |> (fun x -> if plain then x @ [base_class ^ "--plain"] else x))]
                 |> add_common_attrs ?id ?style ?attrs
                 |> (fun x -> if ripple then (a_user_data "mdc-auto-init" "MDCRipple") :: x else x)
                 |> (fun x -> match label with
                              | Some label -> (a_aria "label" [label]) :: x
                              | None       -> x))
             [ span ~a:[a_class [base_class ^ "__icon"]] [pcdata icon] ]

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

    let base_class = "mdc-radio"

    let create_label ?id ?style ?(classes=[]) ?attrs ~id_for ~label () =
      Html.label ~a:([ a_class classes
                     ; a_label_for id_for ]
                     |> add_common_attrs ?id ?style ?attrs)
                 [ pcdata label ]

    let create ?id ?input_id ?style ?(classes=[]) ?attrs ?(js=true) ?(checked=false) ?(disabled=false) ~name () =
      div ~a:([ a_class (base_class :: classes
                         |> fun x -> if disabled then x @ [base_class ^ "--disabled"] else x) ]
              |> (fun x -> if js then (a_user_data "mdc-auto-init" "MDCRadio") :: x else x)
              |> add_common_attrs ?id ?style ?attrs)
          [ input ~a:([ a_class [base_class ^ "__native-control"]
                      ; a_input_type `Radio
                      ; a_name name ]
                      |> add_common_attrs ?id:input_id ?style:None
                      |> (fun x -> if checked then a_checked () :: x else x)
                      |> (fun x -> if disabled then a_disabled () :: x else x))
                  ()
          ; div ~a:[ a_class [base_class ^ "__background"] ]
                [ div ~a:[ a_class [base_class ^ "__outer-circle"]] []
                ; div ~a:[ a_class [base_class ^ "__inner-circle"]] [] ]
          ]

  end

  module Select = struct

    let base_class = "mdc-select"

    let create ?id ?style ?(classes=[]) () =
      div ~a:([ a_class (base_class :: classes)
              ; a_role ["listbox"]
              ; a_tabindex 0 ]
              |> add_common_attrs ?id ?style)
          [ span ~a:[ a_class [base_class ^ "__selected-text"] ] []]

  end

  module Simple_menu = struct

  end

  module Slider = struct

  end

  module Snackbar = struct

    let base_class = "mdc-snackbar"

    let create ?id ?style ?(classes=[]) ?attrs ?(start_aligned=false) () =
      div ~a:([ a_class (base_class :: classes
                         |> fun x -> if start_aligned then x @ [base_class ^ "--align-start"] else x)
              ; a_aria "live" ["assertive"]
              ; a_aria "atomic" ["true"]
              ; a_aria "hidden" ["true"]]
              |> add_common_attrs ?id ?style ?attrs)
          [ div ~a:([a_class [base_class ^ "__text"]]) []
          ; div ~a:([a_class [base_class ^ "__action-wrapper"]])
                [ Button.create ~classes:[base_class ^ "__action-button"] () ]
          ]

  end

  module Switch = struct

    let base_class = "mdc-switch"

    let create_label ?id ?style ?(classes=[]) ?attrs ~id_for ~label () =
      Html.label ~a:([ a_class ((base_class ^ "-label") :: classes)
                     ; a_label_for id_for ]
                     |> add_common_attrs ?id ?style ?attrs)
                 [pcdata label]

    let create ?id ?input_id ?style ?(classes=[]) ?attrs ?(disabled=false) () =
      div ~a:([a_class (base_class :: classes
                        |> fun x -> if disabled then x @ [base_class ^ "--disabled"] else x)]
              |> add_common_attrs ?id ?style ?attrs)
          [ input ~a:([ a_input_type `Checkbox
                      ; a_class [base_class ^ "__native-control"]]
                      |> fun x -> if disabled then (a_disabled ()) :: x else x
                                                                             |> add_common_attrs ?id:input_id ?style:None) ()
          ; div ~a:([ a_class [base_class ^ "__background"]])
                [ div ~a:([ a_class [base_class ^ "__knob"]]) []]
          ]

  end

  module Tabs = struct

    type tab_content = Text of string
                     | Icon of (string * string option)
                     | Text_and_icon of (string * string)

    let base_class = "mdc-tab-bar"

    let tab_class = "mdc-tab"

    let create_tab ?(classes=[]) ?id ?style ?attrs ?(active=false) ?href ~content () =
      a ~a:([a_class (tab_class :: classes
                      |> (fun l -> if active then (tab_class ^ "--active") :: l else l))]
            |> add_common_attrs ?id ?style ?attrs
            |> (fun l -> match href with
                         | Some x -> l @ [a_href x]
                         | None   -> l))
        (match content with
         | Text s -> [pcdata s]
         | Icon (i,fallback) -> [Html.i ~a:([a_class ["material-icons"; (tab_class ^ "__icon")]]
                                            |> (fun l -> match fallback with
                                                         | Some x -> l @ [a_aria "label" [x]]
                                                         | None   -> l))
                                        [pcdata i]]
         | Text_and_icon (s,i) -> [ Html.i ~a:([ a_class ["material-icons"; (tab_class ^ "__icon")]
                                               ; a_aria "hidden" ["true"]])
                                           [pcdata i]
                                  ; span ~a:[a_class [(tab_class ^ "__icon-text")]]
                                         [pcdata s]])

    let create_indicator ?(classes=[]) ?id ?style ?attrs () =
      span ~a:([a_class ((base_class ^ "__indicator") :: classes)]
               |> add_common_attrs ?id ?style ?attrs) []

    let create ?(classes=[]) ?id ?style ?attrs
               ?(with_indicator=true) ?(accent_indicator=false) ~_type ~children () =
      nav ~a:([ a_class (base_class :: classes
                         |> (fun l -> match _type with
                                      | `Text          -> l
                                      | `Icon          -> (base_class ^ "--icon-tab-bar") :: l
                                      | `Text_and_icon -> (base_class ^ "--icons-with-text") :: l)
                         |> (fun l -> if accent_indicator
                                      then (base_class ^ "--indicator-accent") :: l
                                      else l))
              ; a_user_data "mdc-auto-init" "MDCTabBar"]
              |> add_common_attrs ?id ?style ?attrs)
          (children |> (fun x -> if with_indicator then (x @ [create_indicator ()]) else x))

  end

  module Text_field = struct

    let base_class = "mdc-textfield"

    let create ?(classes=[]) ?id ?style ?attrs ?(password=false) ?(disabled=false) label () =
      Html.label ~a:([ a_class (base_class :: classes)
                     ; a_user_data "mdc-auto-init" "MDCTextfield" ]
                     |> add_common_attrs ?id ?style ?attrs)
                 [ input ~a:([ a_class [base_class ^ "__input"]
                             ; a_input_type (if password then `Password else `Text) ]
                             |> (fun l -> if disabled then l @ [a_disabled ()] else l))
                         ()
                 ; span ~a:[a_class [base_class ^ "__label"]]
                        [pcdata label]]

  end

  module Toolbar = struct

  end

end
