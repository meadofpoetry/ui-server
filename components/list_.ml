open Widget
open Markup
open Tyxml_js

module Divider = struct
  class t ?inset () = object
    inherit [Dom_html.element Js.t] widget (List_.Item.create_divider ?inset () |> To_dom.of_element) ()
  end
end

module Item = struct

  (* FIXME add ripple manually, without auto-init? *)

  class t ?ripple ?secondary_text ?start_detail ?end_detail ~text () =

    let elt =
      List_.Item.create ?auto_init:ripple
                        ?secondary_text
                        ?start_detail:(CCOpt.map (fun x -> Of_dom.of_element x#element) start_detail)
                        ?end_detail:(CCOpt.map (fun x -> Of_dom.of_element x#element) end_detail)
                        ~text ()
      |> To_dom.of_element in

    object

      inherit [Dom_html.element Js.t] widget elt ()

      method text           = text
      method secondary_text = secondary_text

      initializer
        CCOpt.iter (fun x -> x#add_class List_.Item.start_detail_class) start_detail;
        CCOpt.iter (fun x -> x#add_class List_.Item.end_detail_class) end_detail

    end

end

class t ?avatar ~(items:[ `Item of Item.t | `Divider of Divider.t ] list) () =

  let two_line = CCOpt.is_some @@ CCList.find_pred (function
                                                    | `Divider _ -> false
                                                    | `Item x    -> CCOpt.is_some x#secondary_text)
                                                   items in

  let elt = List_.create ?avatar ~two_line ~items:(List.map (function
                                                             | `Divider x -> Of_dom.of_element x#root
                                                             | `Item x    -> Of_dom.of_element x#root)
                                                            items) ()
            |> To_dom.of_element in

  object

    inherit [Dom_html.element Js.t] widget elt () as super

    method dense        = super#add_class List_.dense_class
    method bordered     = super#add_class List_.bordered_class
    method not_dense    = super#remove_class List_.dense_class
    method not_bordered = super#remove_class List_.bordered_class

    method items : Item.t list = CCList.filter_map (function `Item x -> Some x | `Divider _ -> None) items

  end

module List_group = struct

  type group =
    { subheader : string option
    ; list      : t
    }

  module Subheader = struct
    class t ~text () = object
      inherit [Dom_html.element Js.t] widget (List_.List_group.create_subheader ~text () |> To_dom.of_element) ()
    end
  end

  let rec add_dividers acc l =
    match l with
    | []       -> acc
    | hd :: [] -> List.rev @@ hd :: acc
    | hd :: tl -> add_dividers ((hd @ [Of_dom.of_element (new Divider.t ())#element]) :: acc) tl

  class t ?(dividers=true) ~(content:group list) () =

    let content = List.map (fun gp -> (CCOpt.map (fun x -> new Subheader.t ~text:x ()) gp.subheader, gp.list))
                           content in

    let elt = List_.List_group.create
                ~content:(CCList.map (fun (h,l) -> let h = CCOpt.map (fun x -> Of_dom.of_element x#element) h in
                                                    [Of_dom.of_element l#element]
                                                    |> CCList.cons_maybe h)
                                     content
                          |> (fun x -> if dividers then add_dividers [] x else x)
                          |> CCList.flatten)
                ()
              |> To_dom.of_div in

    object

      inherit [Dom_html.divElement Js.t] widget elt ()

      method content    = content
      method lists      = CCList.map (fun (_,l) -> l) content
      method subheaders = CCList.map (fun (h,_) -> h) content

    end

end

