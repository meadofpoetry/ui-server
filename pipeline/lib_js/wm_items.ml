open Js_of_ocaml
open Containers
open Components
open Wm_types
open Wm_components

let drag_type_prefix = "application/grid_item-"

module Make(I : Item) = struct

  let base_class = "wm-items"

  module Add = struct

    let base_class = Markup.CSS.add_element base_class "add"
    let item_class = Markup.CSS.add_element base_class "item"
    let wrapper_class = Markup.CSS.add_element base_class "wrapper"

    class item ~candidate ~candidates ~set_candidates ~widgets () =
      let data = I.to_yojson candidate
                 |> Yojson.Safe.to_string
                 |> Js.string in
      let wh = match I.size_of_t candidate with
        | Some w, Some h -> string_of_int w ^ ":" ^ string_of_int h
        | Some w, None   -> string_of_int w ^ ":" ^ "null"
        | None, Some h   -> "null" ^ ":" ^ string_of_int h
        | None, None     -> "null:null" in
      let typ = drag_type_prefix ^ wh in
      let box = new Hbox.t ~widgets () in
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
            super#listen Widget.Event.dragstart (fun _ e ->
                super#style##.opacity := Js.def @@ Js.string "0.5";
                super#style##.zIndex  := Js.string "5";
                e##.dataTransfer##setData (Js.string typ) data;
                true) in
          let dragend =
            super#listen Widget.Event.dragend (fun _ e ->
              let res = e##.dataTransfer##.dropEffect |> Js.to_string in
              if (not @@ String.equal res "none") && candidate.unique
              then (let cs = React.S.value candidates in
                    let c  = List.filter (fun x -> not @@ I.equal x candidate)
                               cs in
                    set_candidates c);
              box#style##.opacity := Js.def @@ Js.string "";
              box#style##.zIndex  := Js.string "";
              false) in
          _dragstart <- Some dragstart;
          _dragend <- Some dragend

        method! destroy () : unit =
          super#destroy ();
          Option.iter Dom_events.stop_listen _dragstart;
          _dragstart <- None;
          Option.iter Dom_events.stop_listen _dragend;
          _dragend <- None
      end

    let make_item candidates set_candidates (candidate : I.t) =

      let text = new Typography.Text.t
                   ~adjust_margin:false
                   ~text:candidate.name () in
      let box  = new item
                   ~candidate
                   ~candidates
                   ~set_candidates
                   ~widgets:[candidate.icon; text#widget] () in
      box

    let make ~candidates ~set_candidates () =
      let ph =
        Placeholder.make
          ~text:"Нет доступных виджетов"
          ~icon:Icon.SVG.(create_simple Path.information) () in
      let wrapper = Tyxml_js.Html.(div ~a:[a_class [wrapper_class]] [])
                    |> Tyxml_js.To_dom.of_element
                    |> Widget.create  in
      let card = new Card.t ~widgets:[wrapper] () in
      let () = card#add_class base_class in
      let _ =
        React.S.map (function
            | [] -> wrapper#set_empty ();
                    wrapper#append_child ph
            | l -> wrapper#set_empty ();
                   List.iter wrapper#append_child
                   @@ List.map (make_item candidates set_candidates) l)
          candidates
      in
      card

  end

  module Properties = struct

    let base_class = Markup.CSS.add_element base_class "properties"

    let make widgets (s : I.t Dynamic_grid.Item.t option React.signal) =
      let ph = Placeholder.make
                 ~text:"Выберите элемент в раскладке"
                 ~icon:Icon.SVG.(create_simple Path.gesture_tap) () in
      let card = new Card.t ~widgets:[] () in
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
                      let b = new Button.t ~label () in
                      b#listen Widget.Event.click (fun _ _ ->
                          on_click (); true) |> ignore;
                      b) w.actions
                in
                let buttons = new Card.Actions.Buttons.t ~widgets:l () in
                let actions = new Card.Actions.t ~widgets:[buttons] () in
                actions#set_id actions_id;
                w.widget#set_id id;
                card#append_child w.widget;
                card#append_child actions
             | None -> card#append_child ph))
          s in
      card#add_class base_class;
      card

  end

  let make ~selected ~candidates ~set_candidates () =
    let add_title = "Добавить" in
    let props_title = "Свойства" in
    let add = Add.make ~candidates ~set_candidates () in
    let props = Properties.make [] selected in
    let title = Wm_selectable_title.make [ add_title, add
                                         ; props_title, props ] in
    let box = new Vbox.t ~widgets:[ add#widget; props#widget ] () in
    let box = new Vbox.t ~widgets:[ title#widget; box#widget ] () in
    let sel = function
      | `Add -> title#select_by_name add_title
      | `Props -> title#select_by_name props_title in
    box#add_class base_class;
    box, sel

end
