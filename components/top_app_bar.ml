open Containers
open Tyxml_js

module Markup = Components_markup.Top_app_bar.Make(Xml)(Svg)(Html)

type align = [`Start | `End] [@@deriving eq]

let align_to_class : align -> string = function
  | `Start -> Markup.section_align_start_class
  | `End -> Markup.section_align_end_class

module Section = struct

  class t ?(align : align option)  ~(widgets : #Widget.t list) () =
    let content = List.map Widget.to_markup widgets in
    let elt = Markup.create_section ?align ~content ()
              |> To_dom.of_element in
    object
      val mutable align : align option = align

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ()

      method align : align option =
        align

      method set_align (x : align option) : unit =
        if not @@ (Equal.option equal_align) align x then
          (Option.iter Fun.(super#remove_class % align_to_class) align;
           Option.iter Fun.(super#add_class % align_to_class) x;
           align <- x)

    end

end

module Row = struct

end

