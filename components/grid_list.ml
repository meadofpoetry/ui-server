open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Grid_list.Make(Xml)(Svg)(Html)

module Tile = struct

  module Primary = struct

    class t ?src ?alt ?(is_div=false) () =

      let content =
        new Widget.t (Markup.Tile.Primary.create_content ?src ?alt ~is_div ()
                      |> To_dom.of_element) () in
      let (elt : Dom_html.element Js.t) =
        Markup.Tile.Primary.create ~content:(Widget.to_markup content) ()
        |> To_dom.of_div in
      object

        val content_widget = content
        val mutable src = src

        inherit Widget.t elt ()
        method content_widget = content_widget

        method src = src
        method set_src s =
          if is_div
          then content_widget#style##.backgroundImage := Js.string ("url(" ^ s ^ ")")
          else (Js.Unsafe.coerce content_widget#root)##.src := Js.string s;
          src <- Some s

      end

  end

  module Caption = struct

    class title ~title () = object
      inherit Widget.t (Markup.Tile.Caption.create_title ~text:title ()
                        |> To_dom.of_element) () as super

      method text = super#text_content |> Option.get_or ~default:""
      method set_text s = super#set_text_content s
    end

    class support_text ~support_text () = object
      inherit Widget.t (Markup.Tile.Caption.create_support_text ~text:support_text ()
                        |> To_dom.of_element) () as super
      method text = super#text_content |> Option.get_or ~default:""
      method set_text s = super#set_text_content s
    end

    class t ?title ?support_text ?icon () =
      let title_widget = Option.map (fun x -> new title ~title:x ()) title in
      let support_text_widget =
        Option.map (fun x -> new support_text ~support_text:x ()) support_text in
      let (elt : Dom_html.element Js.t) =
        Markup.Tile.Caption.create
          ?title:(Option.map Widget.to_markup title_widget)
          ?support_text:(Option.map Widget.to_markup support_text_widget)
          ?icon:(Option.map Widget.to_markup icon)
          ()
        |> To_dom.of_element in

      object(self)

        val title_widget = title_widget
        val support_text_widget = support_text_widget

        inherit Widget.t elt () as super

        method! init () : unit =
          super#init ();
          Option.iter (fun x -> x#add_class Markup.Tile.Caption.icon_class) icon

        method title_widget = title_widget
        method support_text_widget = support_text_widget

        method support_text : string option =
          Option.map (fun x -> x#text) self#support_text_widget
        method set_support_text (s : string) : unit =
          Option.iter (fun x -> x#set_text s) self#support_text_widget

        method title : string option =
          Option.map (fun x -> x#text) self#title_widget
        method set_title (s : string) : unit =
          Option.iter (fun x -> x#set_text s) self#title_widget
      end

  end

  class t ?src ?icon ?title ?support_text () =

    let caption_widget = match icon,title,support_text with
      | None, None, None -> None
      | _ -> Some (new Caption.t ?title ?support_text ?icon ()) in
    let primary_widget = new Primary.t ~is_div:true ?src () in
    let (elt : Dom_html.element Js.t) =
      Markup.Tile.create ~primary:(Widget.to_markup primary_widget)
        ?caption:(Option.map Widget.to_markup caption_widget)
        ()
      |> To_dom.of_element in

    object

      val caption_widget = caption_widget
      val primary_widget = primary_widget

      inherit Widget.t elt ()

      method caption_widget = caption_widget
      method primary_widget = primary_widget

    end

end

type ar = [ `AR_1_1 | `AR_16_9 | `AR_2_3 | `AR_3_2 | `AR_4_3 | `AR_3_4 ]

class t ~(tiles : Tile.t list) () =

  let (twoline : bool) =
    List.find_pred (fun x ->
        match x#caption_widget with
        | None -> false
        | Some c -> Option.is_some c#support_text_widget) tiles
    |> Option.is_some in
  let (elt : Dom_html.element Js.t) =
    Markup.create ~tiles:(List.map Widget.to_markup tiles) ()
    |> To_dom.of_div in

  object(self)

    val tiles = tiles
    val mutable ar : ar option = None

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      if twoline then super#add_class Markup.twoline_caption_class

    method tiles = tiles

    method ar = ar
    method set_ar : ar option -> unit = function
      | Some ar ->
         self#set_ar None;
         super#add_class @@ Markup.ar_to_class ar
      | None -> Option.iter Fun.(super#remove_class % Markup.ar_to_class) ar

    method set_one_px_gutter (x : bool) : unit =
      self#add_or_remove_class x Markup.tile_gutter_1_class

    method set_caption_as_header (x : bool) : unit =
      self#add_or_remove_class x Markup.header_caption_class

    method set_icon_align_start () : unit =
      super#remove_class Markup.icon_align_end_class;
      super#add_class Markup.icon_align_start_class

    method set_icon_align_end () : unit =
      super#remove_class Markup.icon_align_start_class;
      super#add_class Markup.icon_align_end_class
  end
