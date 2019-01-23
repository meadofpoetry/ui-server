open Common.User
open Api.Template
open Common.Uri
open Tyxml

module Icon = Components_markup.Icon.Make(Xml)(Svg)(Html)

module Tab = Components_markup.Tab.Make(Xml)(Svg)(Html)
module Tab_bar = Components_markup.Tab_bar.Make(Xml)(Svg)(Html)
module Tab_scroller = Components_markup.Tab_scroller.Make(Xml)(Svg)(Html)
module Tab_indicator = Components_markup.Tab_indicator.Make(Xml)(Svg)(Html)

let make_icon path =
  let open Icon.SVG in
  let path = create_path path () in
  let icon = create [path] () in
  Tyxml.Html.toelt icon

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

let create () : upper ordered_item list user_table =
  let id = "mosaic" in
  let app_bar =
    make_app_bar_props ~title:"Мозаика"
      (* ~bottom:(make_tab_bar ()) *)
      () in
  let template =
    make_tmpl_props ~id ~app_bar
      ~pre_scripts:[ Src "/js/janus.nojquery.js"
                   ; Src "/js/adapter.min.js" ]
      ~post_scripts:[Src "/js/page_mosaic.js"]
      ~stylesheets:["/css/pipeline.min.css"]
      () in
  let rval =
    [`Index 1,
     Simple { id
            ; title = "Мозаика"
            ; icon = Some (make_icon Icon.SVG.Path.collage)
            ; href = Path.of_string "pipeline"
            ; template }
    ]
  in
  { root = rval; operator = rval; guest = rval
  }
