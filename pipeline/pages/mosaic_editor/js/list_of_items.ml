open Js_of_ocaml
open Js_of_ocaml_tyxml
open Components
open Wm_types
open Basic_widgets

let drag_type_prefix = "application/grid_item-"

module Make(I : Item) = struct

  let base_class = "wm-items"

  module Add = struct

    let base_class = Components_tyxml.BEM.add_element base_class "add"
    let item_class = Components_tyxml.BEM.add_element base_class "item"
    let wrapper_class = Components_tyxml.BEM.add_element base_class "wrapper"

    class item ~candidate ~candidates ~set_candidates ~widgets () =
      let data = I.to_yojson candidate
                 |> Yojson.Safe.to_string
                 |> Js.string in
      let wh = match I.size_of_t candidate with
        | Some w, Some h -> string_of_int w ^ ":" ^ string_of_int h
        | Some w, None -> string_of_int w ^ ":" ^ "null"
        | None, Some h -> "null" ^ ":" ^ string_of_int h
        | None, None -> "null:null" in
      let typ = drag_type_prefix ^ wh in
      let box = Box.make ~dir:`Row widgets in
      object
        inherit Widget.t box#root () as super
        inherit Touch_draggable.t ~data ~typ box#root ()

        val mutable _dragstart = None
        val mutable _dragend = None

        method! init () : unit =
          super#init ();
          super#set_attribute "draggable" "true";
          super#add_class item_class;

          let dragstart =
            Events.dragstarts super#root (fun e _ ->
                super#root##.style##.opacity := Js.def @@ Js.string "0.5";
                super#root##.style##.zIndex  := Js.string "5";
                e##.dataTransfer##setData (Js.string typ) data;
                Lwt.return_unit) in
          let dragend =
            Events.dragends super#root (fun e _ ->
                let res = e##.dataTransfer##.dropEffect |> Js.to_string in
                if (not @@ String.equal res "none") && candidate.unique
                then (let cs = React.S.value candidates in
                      let c  = List.filter (fun x -> not @@ I.equal x candidate)
                          cs in
                      set_candidates c);
                box#root##.style##.opacity := Js.def @@ Js.string "";
                box#root##.style##.zIndex  := Js.string "";
                Dom.preventDefault e;
                Lwt.return_unit) in
          _dragstart <- Some dragstart;
          _dragend <- Some dragend

        method! destroy () : unit =
          super#destroy ();
          Utils.Option.iter Lwt.cancel _dragstart;
          _dragstart <- None;
          Utils.Option.iter Lwt.cancel _dragend;
          _dragend <- None
      end

    let make_item candidates set_candidates (candidate : I.t) =

      let text = Typography.Text.make candidate.name in
      let box = new item
        ~candidate
        ~candidates
        ~set_candidates
        ~widgets:[candidate.icon; text#widget] () in
      box

    let make ~candidates ~set_candidates () =
      let ph =
        Placeholder.make
          ~text:"Нет доступных виджетов"
          ~icon:(Icon.SVG.make_simple Icon.SVG.Path.information)
          () in
      let wrapper = Tyxml_js.Html.(div ~a:[a_class [wrapper_class]] [])
                    |> Tyxml_js.To_dom.of_element
                    |> Widget.create in
      let _ =
        React.S.map (function
            | [] -> wrapper#remove_children ();
                    wrapper#append_child ph
            | l -> wrapper#remove_children ();
                   List.iter wrapper#append_child
                   @@ List.map (make_item candidates set_candidates) l)
          candidates
      in
      wrapper

  end

  module Properties = struct

    let base_class = Components_tyxml.BEM.add_element base_class "properties"

    let make widgets (s : I.t Dynamic_grid.Item.t option React.signal) =
      let ph = Placeholder.make
          ~text:"Выберите элемент в раскладке"
          ~icon:Icon.SVG.(make_simple Path.gesture_tap)
          () in
      let card = Card.make [] in
      let id = "wm-item-properties" in
      let actions_id = "wm-item-properties-actions" in
      let _ =
        React.S.map (fun selected ->
            (try
               Dom.removeChild card#root (Dom_html.getElementById id);
               Dom.removeChild card#root (Dom_html.getElementById actions_id)
             with _ -> ());
            (match selected with
             | Some x ->
                card#remove_child ph;
                let w = I.make_item_properties x#s_value x#set_value widgets in
                let l =
                  List.map (fun { label; on_click } ->
                      Button.make
                        ~label
                        ~on_click:(fun _ _ _ -> on_click (); Lwt.return_unit)
                        ()) w.actions in
                let buttons = Card.Actions.make_buttons l in
                let actions = Card.Actions.make [buttons] in
                actions#root##.id := Js.string actions_id;
                w.widget#root##.id := Js.string id;
                card#append_child w.widget;
                card#append_child actions
             | None -> card#append_child ph))
          s in
      card#add_class base_class;
      card

  end

  let make ~selected ~candidates ~set_candidates () =
    let add = Add.make ~candidates ~set_candidates () in
    add#add_class base_class;
    add

end
