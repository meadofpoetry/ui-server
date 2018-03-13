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

    class item ~candidate ~candidates ~set_candidates ~widgets () = object(self)

      inherit Box.t ~vertical:false ~widgets ()

      initializer
        Dom_events.listen self#root Dom_events.Typ.dragstart
                          (fun _ e -> let s = I.to_yojson candidate
                                              |> Yojson.Safe.to_string
                                              |> Js.string
                                      in
                                      let wh = match I.size_of_t candidate with
                                        | Some w, Some h -> string_of_int w ^ ":" ^ string_of_int h
                                        | Some w, None   -> string_of_int w ^ ":" ^ "null"
                                        | None, Some h   -> "null" ^ ":" ^ string_of_int h
                                        | None, None     -> "null:null"
                                      in
                                      let typ = drag_type_prefix ^ wh in
                                      e##.dataTransfer##setData (Js.string typ) s;
                                      self#style##.opacity := Js.def @@ Js.string "0.5";
                                      self#style##.zIndex := Js.string "5";
                                      true)
        |> ignore;
        Dom_events.listen self#root Dom_events.Typ.dragend
                          (fun _ e ->
                            let res = e##.dataTransfer##.dropEffect |> Js.to_string in
                            if (not @@ String.equal res "none") && candidate.unique
                            then (let cs = React.S.value candidates in
                                  let c = List.filter (fun x -> not @@ I.equal x candidate) cs in
                                  set_candidates c);
                            self#style##.opacity := Js.def @@ Js.string "";
                            self#style##.zIndex := Js.string "";
                            false)
        |> ignore;
        self#add_class item_class;
        self#set_attribute "draggable" "true"

    end

    let make_item candidates set_candidates (candidate : I.t) =

      let icon = new Icon.Font.t ~icon:candidate.icon () in
      let text = new Typography.Text.t ~adjust_margin:false ~text:candidate.name () in
      let box  = new item ~candidate ~candidates ~set_candidates ~widgets:[icon#widget;text#widget] () in
      box

    let make ~candidates ~set_candidates () =
      let card   = new Card.t ~widgets:[] () in
      let ()     = card#add_class base_class in
      let _      = React.S.map (fun l -> Utils.rm_children card#root;
                                         let items  = List.map (fun x -> make_item candidates set_candidates x) l
                                         in
                                         List.iter (fun x -> Dom.appendChild card#root x#root) items)
                               candidates
      in
      card

  end

  module Properties = struct

    let base_class = Markup.CSS.add_element base_class "properties"

    let make widgets (s : I.t Dynamic_grid.Item.t option React.signal) =
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
                               let w = I.make_item_properties x#get_value widgets x#set_value in
                               let l = List.map (fun {label;on_click} ->
                                           let b = new Button.t ~label () in
                                           let _ = React.E.map (fun _ -> on_click ()) b#e_click in
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

  let make ~selected ~candidates ~set_candidates () =
    let add_title   = "Добавить" in
    let props_title = "Свойства" in
    let add     = Add.make ~candidates ~set_candidates () in
    let props   = Properties.make [] selected in
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
