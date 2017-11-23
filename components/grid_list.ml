open Widget
open Markup
open Tyxml_js

module Tile = struct

  module Primary = struct

    class content ?src ?alt ?is_div () = object
      inherit [Dom_html.element Js.t] widget (Grid_list.Tile.Primary.create_content ?src ?alt ?is_div ()
                                              |> To_dom.of_element) ()
    end

    class t ?src ?alt ?(is_div=false) () =

      let content = new content ?src ?alt ~is_div () in

      let elt = Grid_list.Tile.Primary.create ~content:(Of_dom.of_element content#element) ()
                |> To_dom.of_div in

      object

        val content_widget = content
        val mutable src = src

        inherit [Dom_html.element Js.t] widget elt ()

        method content_widget = content_widget

        method src       = src
        method set_src s = if is_div
                           then content_widget#style##.backgroundImage := Js.string ("url(" ^ s ^ ")")
                           else (Js.Unsafe.coerce content_widget#root)##.src := Js.string s;
                           src <- Some s

      end

  end

  module Caption = struct

    class title ~title () = object
      inherit [Dom_html.element Js.t] widget (Grid_list.Tile.Caption.create_title ~text:title ()
                                              |> To_dom.of_element) () as super
      method text       = super#text_content
      method set_text s = super#set_text_content s
    end

    class support_text ~support_text () = object
      inherit [Dom_html.element Js.t] widget (Grid_list.Tile.Caption.create_support_text ~text:support_text ()
                                              |> To_dom.of_element) () as super

      method text       = super#text_content
      method set_text s = super#set_text_content s
    end

    class t ?title ?support_text ?icon () =

      let title_widget           = CCOpt.map (fun x -> new title ~title:x ()) title in
      let support_text_widget = CCOpt.map (fun x -> new support_text ~support_text:x ()) support_text in
      let () = CCOpt.iter (fun x -> x#add_class Grid_list.Tile.Caption.icon_class) icon in

      let map_opt_to_elt x = CCOpt.map (fun x -> Of_dom.of_element x#element) x in
      let elt = Grid_list.Tile.Caption.create
                  ?title:(map_opt_to_elt title_widget)
                  ?support_text:(map_opt_to_elt support_text_widget)
                  ?icon:(map_opt_to_elt icon)
                  ()
                |> To_dom.of_element in

      object(self)

        val title_widget        = title_widget
        val support_text_widget = support_text_widget

        inherit [Dom_html.element Js.t] widget elt ()

        method title_widget        = title_widget
        method support_text_widget = support_text_widget

        method support_text = CCOpt.map (fun x -> x#text) self#support_text_widget
        method title        = CCOpt.map (fun x -> x#text) self#title_widget

        method set_support_text s = CCOpt.iter (fun x -> x#set_text s) self#support_text_widget
        method set_title        s = CCOpt.iter (fun x -> x#set_text s) self#title_widget

      end

  end

  class t ?src ?icon ?title ?support_text () =

    let caption_widget = (match icon,title,support_text with
                          | None,None,None -> None
                          | _              -> Some (new Caption.t ?title ?support_text ?icon ())) in
    let primary_widget = new Primary.t ~is_div:true ?src () in
    let elt = Grid_list.Tile.create ~primary:(Of_dom.of_element primary_widget#element)
                                    ?caption:(CCOpt.map (fun x -> Of_dom.of_element x#element) caption_widget)
                                    ()
              |> To_dom.of_element in

    object

      val caption_widget = caption_widget
      val primary_widget = primary_widget

      inherit [Dom_html.liElement Js.t] widget elt ()

      method caption_widget = caption_widget
      method primary_widget = primary_widget

    end

end

type ar = [ `AR_1_1 | `AR_16_9 | `AR_2_3 | `AR_3_2 | `AR_4_3 | `AR_3_4 ]

class t ~(tiles:Tile.t list) () =

  let twoline = CCList.find_pred (fun x -> match x#caption_widget with
                                           | Some c -> CCOpt.is_some c#support_text
                                           | None   -> false) tiles
                |> CCOpt.is_some in

  let elt = Grid_list.create ~tiles:(List.map (fun x -> Of_dom.of_element x#element) tiles) ()
            |> To_dom.of_div in

  object(self)

    val tiles = tiles
    val mutable ar : ar option = None

    inherit [Dom_html.divElement Js.t] widget elt () as super

    method tiles = tiles

    method ar            = ar
    method remove_ar     = CCOpt.iter (fun x -> super#remove_class @@ Grid_list.ar_to_class x) ar
    method set_ar (x:ar) = self#remove_ar; super#add_class @@ Grid_list.ar_to_class x

    method one_px_gutter  = super#add_class Grid_list.tile_gutter_1_class
    method default_gutter = super#remove_class Grid_list.tile_gutter_1_class

    method caption_as_header = super#add_class Grid_list.header_caption_class
    method caption_as_footer = super#remove_class Grid_list.header_caption_class

    method icon_align_start  = super#remove_class Grid_list.icon_align_end_class;
                               super#add_class Grid_list.icon_align_start_class
    method icon_align_end    = super#remove_class Grid_list.icon_align_start_class;
                               super#add_class Grid_list.icon_align_end_class

    initializer
      if twoline then super#add_class Grid_list.twoline_caption_class

  end
