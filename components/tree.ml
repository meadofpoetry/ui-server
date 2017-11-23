open Widget
open Tyxml_js

module Item = struct

  class t ?ripple ?secondary_text ?start_detail ?nested ~text () =

    let s,s_push = React.S.create false in
    let end_detail = CCOpt.map (fun _ -> let icon = new Icon.Font.t ~icon:"expand_more" () in
                                         React.S.map (fun x -> if x then icon#set_icon "expand_less"
                                                               else icon#set_icon "expand_more") s |> ignore;
                                         icon)
                               nested in
    let item       = new List_.Item.t ?ripple ?secondary_text ?start_detail ?end_detail ~text () in

    let elt = Markup.Tree.Item.create ~item:(Of_dom.of_element item#element)
                                      ?nested_list:(CCOpt.map (fun x -> Of_dom.of_element x#element) nested)
                                      ()
              |> To_dom.of_element in

    object

      inherit [Dom_html.divElement Js.t] widget elt () as super

      method item           = item
      method text           = item#text
      method secondary_text = item#secondary_text

      initializer
        CCOpt.iter (fun x -> x#add_class Markup.Tree.Item.nested_list_hidden_class;
                             item#style##.cursor := Js.string "pointer") nested;
        Dom_events.listen super#root
                          Dom_events.Typ.click
                          (fun _ _ -> let hidden_class = Markup.Tree.Item.nested_list_hidden_class in
                                      CCOpt.iter (fun x -> x#toggle_class hidden_class |> not |> s_push) nested;
                                      true)
        |> ignore;
    end

end

class t ~(items:Item.t list) () =

  let two_line = CCOpt.is_some @@ CCList.find_pred (fun x -> CCOpt.is_some x#secondary_text) items in

  let elt = Markup.Tree.create ~two_line
                               ~items:(List.map (fun x -> Of_dom.of_element x#element) items)
                               ()
            |> To_dom.of_element in

  object

    val mutable items = items

    inherit [Dom_html.element Js.t] widget elt () as super

    method dense        = super#add_class Markup.List_.dense_class
    method bordered     = super#add_class Markup.List_.bordered_class
    method not_dense    = super#remove_class Markup.List_.dense_class
    method not_bordered = super#remove_class Markup.List_.bordered_class

    method items = items

  end
