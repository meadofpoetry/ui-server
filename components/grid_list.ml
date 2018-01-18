module Tile = struct

  module Primary = struct

    class t ?src ?alt ?(is_div=false) () =

      let content = new Widget.widget (Markup.Grid_list.Tile.Primary.create_content ?src ?alt ~is_div ()
                                       |> Tyxml_js.To_dom.of_element) () in
      let elt     = Markup.Grid_list.Tile.Primary.create ~content:(Widget.widget_to_markup content) ()
                    |> Tyxml_js.To_dom.of_div in
      object

        val content_widget = content
        val mutable src = src

        inherit Widget.widget elt ()
        method get_content_widget = content_widget
        method get_src   = src
        method set_src s = if is_div
                           then content_widget#style##.backgroundImage := Js.string ("url(" ^ s ^ ")")
                           else (Js.Unsafe.coerce content_widget#root)##.src := Js.string s;
                           src <- Some s

      end

  end

  module Caption = struct

    class title ~title () = object
      inherit Widget.widget (Markup.Grid_list.Tile.Caption.create_title ~text:title ()
                             |> Tyxml_js.To_dom.of_element) () as super
      method get_text   = super#get_text_content |> CCOpt.get_or ~default:""
      method set_text s = super#set_text_content s
    end

    class support_text ~support_text () = object
      inherit Widget.widget (Markup.Grid_list.Tile.Caption.create_support_text ~text:support_text ()
                             |> Tyxml_js.To_dom.of_element) () as super
      method get_text   = super#get_text_content |> CCOpt.get_or ~default:""
      method set_text s = super#set_text_content s
    end

    class t ?title ?support_text ?icon () =

      let title_widget = CCOpt.map (fun x -> new title ~title:x ()) title in
      let support_text_widget = CCOpt.map (fun x -> new support_text ~support_text:x ()) support_text in
      let elt = Markup.Grid_list.Tile.Caption.create
                  ?title:(CCOpt.map Widget.widget_to_markup title_widget)
                  ?support_text:(CCOpt.map Widget.widget_to_markup support_text_widget)
                  ?icon:(CCOpt.map Widget.widget_to_markup icon)
                  ()
                |> Tyxml_js.To_dom.of_element in

      object(self)

        val title_widget        = title_widget
        val support_text_widget = support_text_widget

        inherit Widget.widget elt ()

        method get_title_widget        = title_widget
        method get_support_text_widget = support_text_widget

        method get_support_text   = CCOpt.map  (fun x -> x#get_text)   self#get_support_text_widget
        method set_support_text s = CCOpt.iter (fun x -> x#set_text s) self#get_support_text_widget

        method get_title   = CCOpt.map  (fun x -> x#get_text)   self#get_title_widget
        method set_title s = CCOpt.iter (fun x -> x#set_text s) self#get_title_widget

        initializer
          CCOpt.iter (fun x -> x#add_class Markup.Grid_list.Tile.Caption.icon_class) icon
      end

  end

  class t ?src ?icon ?title ?support_text () =

    let caption_widget = (match icon,title,support_text with
                          | None,None,None -> None
                          | _              -> Some (new Caption.t ?title ?support_text ?icon ())) in
    let primary_widget = new Primary.t ~is_div:true ?src () in
    let elt = Markup.Grid_list.Tile.create ~primary:(Widget.widget_to_markup primary_widget)
                                           ?caption:(CCOpt.map Widget.widget_to_markup caption_widget)
                                           ()
              |> Tyxml_js.To_dom.of_element in

    object

      val caption_widget = caption_widget
      val primary_widget = primary_widget

      inherit Widget.widget elt ()

      method get_caption_widget = caption_widget
      method get_primary_widget = primary_widget

    end

end

type ar = [ `AR_1_1 | `AR_16_9 | `AR_2_3 | `AR_3_2 | `AR_4_3 | `AR_3_4 ]

class t ~(tiles:Tile.t list) () =

  let twoline = CCList.find_pred (fun x -> match x#get_caption_widget with
                                           | Some c -> CCOpt.is_some c#get_support_text_widget
                                           | None   -> false) tiles |> CCOpt.is_some in
  let elt = Markup.Grid_list.create ~tiles:(Widget.widgets_to_markup tiles) () |> Tyxml_js.To_dom.of_div in

  object(self)

    val tiles = tiles
    val mutable ar : ar option = None

    inherit Widget.widget elt () as super

    method get_tiles = tiles

    method get_ar        = ar
    method remove_ar     = CCOpt.iter (fun x -> super#remove_class @@ Markup.Grid_list.ar_to_class x) ar
    method set_ar (x:ar) = self#remove_ar; super#add_class @@ Markup.Grid_list.ar_to_class x

    method private add_or_rm_class x c = if x then super#add_class c else super#remove_class c
    method set_one_px_gutter x         = self#add_or_rm_class x Markup.Grid_list.tile_gutter_1_class
    method set_caption_as_header x     = self#add_or_rm_class x Markup.Grid_list.header_caption_class
    method set_icon_align_start        = super#remove_class Markup.Grid_list.icon_align_end_class;
                                         super#add_class Markup.Grid_list.icon_align_start_class
    method set_icon_align_end          = super#remove_class Markup.Grid_list.icon_align_start_class;
                                         super#add_class Markup.Grid_list.icon_align_end_class
    initializer
      if twoline then super#add_class Markup.Grid_list.twoline_caption_class

  end
