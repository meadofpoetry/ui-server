open Common.User
open Api.Template
open Tyxml

module Markup = Components_tyxml.Make(Xml)(Svg)(Html)

let make_icon path =
  let open Markup.Icon.SVG in
  let path = create_path path () in
  let icon = create [path] () in
  Tyxml.Html.toelt icon

let make_icon_button path =
  let path = Markup.Icon.SVG.create_path path () in
  let icon = Markup.Icon.SVG.create [path] () in
  Markup.Icon_button.create icon ()

(* let create_tab ?(active = false) (label : string) =
 *   let text = Tab.create_text_label label () in
 *   let content = Tab.create_content [text] () in
 *   let indicator = Tab_indicator.(create ~active (create_content ()) ()) in
 *   Tab.create ~active ~indicator content ()
 * 
 * let create_scroller tabs =
 *   let content = Tab_scroller.create_scroll_content tabs () in
 *   let scroll_area = Tab_scroller.create_scroll_area ~content () in
 *   Tab_scroller.create ~scroll_area ()
 * 
 * let make_tab_bar () =
 *   let video = create_tab ~active:true "Видео" in
 *   let editor = create_tab "Редактор" in
 *   let scroller = create_scroller [video; editor] in
 *   Tyxml.Html.toelt @@ Tab_bar.create ~scroller () *)

let create_video () : 'a item =
  let id = "mosaic-video" in
  let app_bar = make_app_bar_props ~title:"Мозаика" () in
  let template =
    make_tmpl_props ~id ~app_bar
      ~pre_scripts:[ Src "/js/janus.nojquery.js"
                   ; Src "/js/adapter.min.js" ]
      ~post_scripts:[Src "/js/page_mosaic_video.js"]
      ~stylesheets:["/css/pipeline.min.css"]
      () in
  Simple { id
         ; title = "Видео"
         ; icon = Some (make_icon Icon.SVG.Path.video)
         ; href = Common.Uri.Path.of_string "video"
         ; template }

let create_editor () : 'a item =
  let id = "mosaic-editor" in
  let app_bar = make_app_bar_props ~title:"Редактор мозаики" () in
  let template =
    make_tmpl_props ~id ~app_bar
      ~post_scripts:[Src "/js/page_mosaic_editor.js"]
      ~stylesheets:["/css/pipeline.min.css"]
      () in
  Simple { id
         ; title = "Редактор"
         ; icon = Some (make_icon Icon.SVG.Path.view_dashboard)
         ; href = Common.Uri.Path.of_string "editor"
         ; template }

let create_subtree () =
  Subtree { title = "Мозаика"
          ; icon = None
          ; href = Common.Uri.Path.of_string "mosaic"
          ; templates =
              [ (`Index 1, create_video ())
              ; (`Index 2, create_editor ()) ]}

let create () : upper ordered_item list user_table =
  let rval = [`Index 3, create_subtree ()] in
  { root = rval; operator = rval; guest = rval }
