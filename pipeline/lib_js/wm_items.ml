open Containers
open Components
open Wm_types
open Wm_components

let drag_data_type = "add_candidate"

module Make(I : Item) = struct

  let base_class = "wm-items"

  module Add = struct

    let base_class = Markup.CSS.add_element base_class "add"
    let item_class = Markup.CSS.add_element base_class "item"

    class item ~props ~widgets () = object(self)

      inherit Box.t ~vertical:false ~widgets ()

      initializer
        Dom_events.listen self#root Dom_events.Typ.dragstart
                          (fun _ e -> let s = add_candidate_to_yojson props
                                              |> Yojson.Safe.to_string
                                              |> Js.string
                                      in
                                      e##.dataTransfer##setData (Js.string drag_data_type) s;
                                      self#style##.opacity := Js.def @@ Js.string "0.5";
                                      self#style##.zIndex := Js.string "5";
                                      true)
        |> ignore;
        Dom_events.listen self#root Dom_events.Typ.dragend
                          (fun _ _ -> self#style##.opacity := Js.def @@ Js.string "";
                                      self#style##.zIndex := Js.string "";
                                      false)
        |> ignore;
        self#add_class item_class;
        self#set_attribute "draggable" "true"

    end

    let make_item (props : add_candidate) =

      let icon = new Icon.Font.t ~icon:props.icon () in
      let text = new Typography.Text.t ~adjust_margin:false ~text:props.name () in
      let box  = new item ~props ~widgets:[icon#widget;text#widget] () in
      box

    let make () =
      let items  = List.map (fun x -> x,make_item x) I.add_candidates in
      let card   = new Card.t ~widgets:(List.map snd items) () in
      let ()     = card#add_class base_class in
      card

  end

  module Properties = struct

    let base_class = Markup.CSS.add_element base_class "properties"

    let make (s : (string * I.item) Dynamic_grid.Item.t option React.signal) =
      let ph         = Placeholder.make ~text:"Выберите элемент в раскладке" ~icon:"touch_app" () in
      let card       = new Card.t ~widgets:[] () in
      let id         = "wm-item-properties" in
      let actions_id = "wm-item-properties-actions" in
      let ()         = card#add_class base_class in
      let _ = React.S.map (fun selected ->
                  (try
                     Dom.removeChild card#root (Dom_html.getElementById id);
                     Dom.removeChild card#root (Dom_html.getElementById actions_id)
                   with _ -> ());
                  (match selected with
                   | Some x -> (try Dom.removeChild card#root ph#root with _ -> ());
                               let w = I.make_item_props x in
                               let l = List.map (fun {label;on_click} ->
                                           let b = new Button.t ~label () in
                                           let _ = React.E.map (fun _ -> on_click x) b#e_click in
                                           b) w.actions
                               in
                               let buttons = new Card.Actions.Buttons.t ~widgets:l () in
                               let actions = new Card.Actions.t ~widgets:[buttons] () in
                               actions#set_id actions_id;
                               w.widget#set_id id;
                               Dom.appendChild card#root w.widget#root;
                               Dom.appendChild card#root actions#root
                   | None   -> Dom.appendChild card#root ph#root))
                          s
      in
      card

  end

  let make selected =
    let add_title   = "Добавить" in
    let props_title = "Свойства" in
    let add     = Add.make () in
    let props   = Properties.make selected in
    let title   = Wm_selectable_title.make [ add_title, add
                                        ; props_title, props ] in
    let box     = new Box.t ~vertical:true ~widgets:[add#widget; props#widget] () in
    let ()      = box#add_class base_class in
    let box     = new Box.t ~vertical:true ~widgets:[title#widget; box#widget] () in
    let sel     = function
      | `Add   -> title#select_by_name add_title
      | `Props -> title#select_by_name props_title
    in
    box,sel

end
