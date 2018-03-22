open Containers
open Components
open Wm_types
open Wm_components

let drag_type_prefix = "application/grid_item-"

module Make(I : Item) = struct

  let base_class = "wm-items"

  module Add = struct

    let touch e = Js.Optdef.get (e##.changedTouches##item 0)
                    (fun () -> failwith "touch fail")

    (* imitates an event of a given type, using touch and event target parameters. sets dataTransfer on demand *)
    let dispatch ?data touch typ target =
      let evt      = Js.Unsafe.pure_js_expr "document.createEvent('Event')" in
      let coerced  = Js.Unsafe.coerce evt in
      let optional = match typ with | "dragend" -> Js._false | _ -> Js._true in
      let () = coerced##initEvent (Js.string typ) Js._true optional in
      let () = Js.Unsafe.set evt "dataTransfer" (Js.Unsafe.pure_js_expr "new DataTransfer()") in
      coerced##.button  := Js.string "0";
      coerced##.which   := Js.string "1";
      coerced##.buttons := Js.string "1";
      coerced##.pageX   := touch##.pageX;
      coerced##.pageY   := touch##.pageY;
      coerced##.clientX := touch##.clientX;
      coerced##.clientY := touch##.clientY;
      coerced##.screenX := touch##.screenX;
      coerced##.screenY := touch##.screenY;
      (match data with
       | Some data -> let typ, data = data in
                      coerced##.dataTransfer##setData typ data
       | None      -> ());
      let () = (Js.Unsafe.coerce target)##dispatchEvent evt in
      ()

    (* finds element above the touched point, actually returns Dom_html.element Js.opt *)
    let elt_from_point x y = (Js.Unsafe.coerce Dom_html.document)##elementFromPoint x y


    let base_class = Markup.CSS.add_element base_class "add"
    let item_class = Markup.CSS.add_element base_class "item"

    class draggable ~candidate elt () = object

      val mutable id    = None
      val mutable drag  = false
      val mutable clone = None
      val mutable delta = 0,0

      initializer
      let data = I.to_yojson candidate
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
      let ending e = id   <- None;
                     drag <- false;
                     let touch = touch e in
                     Js.Opt.iter (elt_from_point touch##.clientX touch##.clientY)
                       (fun x -> dispatch touch "drop" x ?data:(Some ((Js.string typ),data)));
                     Option.iter (fun cln ->
                         (try Dom.removeChild Dom_html.document##.body cln with _ -> ());
                         clone <- None) clone;
                     dispatch touch "dragend" elt
          in
          Dom_events.listen elt Dom_events.Typ.dragstart
            (fun _ e -> elt##.style##.opacity := Js.def @@ Js.string "0.5";
                        elt##.style##.zIndex  := Js.string "5";
                        e##.dataTransfer##setData (Js.string typ) data;
                        true) |> ignore;

          Dom_events.listen elt Dom_events.Typ.touchstart
            (fun _ e -> let touch = touch e in
                        let rect  = (Js.Unsafe.coerce elt)##getBoundingClientRect in
                        let del_x, del_y = touch##.pageX - rect##.left, 0 in
                        delta <- del_x, del_y;
                        id    <- Some touch##.identifier;
                        false) |> ignore;

          Dom_events.listen Dom_html.window Dom_events.Typ.touchmove
            (fun _ e -> let touch = touch e in
                        (match drag, id with
                         | false , Some _ ->
                            dispatch touch "dragstart" elt;
                            drag  <- true;
                            let cln = (Js.Unsafe.coerce elt)##cloneNode true in
                            cln##.style##.width    :=
                              Js.string @@ (string_of_int elt##.offsetWidth)^"px";
                            cln##.style##.position := Js.string "absolute";
                            cln##.style##.pointerEvents := Js.string "none";
                            cln##.style##.opacity  := Js.def @@ Js.string "0.5";
                            cln##.style##.zIndex   := Js.string "9999";
                            clone <- Some cln;
                            Dom.appendChild Dom_html.document##.body cln
                         | true, Some id ->
                            if touch##.identifier = id
                            then Js.Opt.iter (elt_from_point touch##.clientX touch##.clientY)
                                   (fun x -> dispatch touch "dragover" x ?data:(Some ((Js.string typ),data)));
                            Option.iter (fun cln ->
                                let dx, dy = delta in
                                cln##.style##.left :=
                                  Js.string @@ (string_of_int (touch##.pageX - dx))^"px";
                                cln##.style##.top  :=
                                  Js.string @@ (string_of_int (touch##.pageY - dy))^"px") clone
                         | _, _ -> ());
                        false) |> ignore;

          Dom_events.listen elt Dom_events.Typ.touchend
            (fun _ e -> ending e; false) |> ignore;

          Dom_events.listen elt Dom_events.Typ.touchcancel
            (fun _ e -> ending e; false) |> ignore;
    end

    class item ~candidate ~candidates ~set_candidates ~widgets () =
      let box  = new Box.t ~vertical:false ~widgets () in
      object(self)
        inherit Widget.widget box#root ()
        inherit draggable ~candidate box#root ()

        initializer
          self#set_attribute "draggable" "true";
          self#add_class item_class;

          Dom_events.listen box#root Dom_events.Typ.dragend
            (fun _ e -> let res = e##.dataTransfer##.dropEffect |> Js.to_string in
                         if (not @@ String.equal res "none") && candidate.unique
                         then (let cs = React.S.value candidates in
                               let c = List.filter (fun x -> not @@ I.equal x candidate) cs in
                               set_candidates c);
                        box#style##.opacity := Js.def @@ Js.string "";
                        box#style##.zIndex  := Js.string "";
                        false) |> ignore;
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
                               let w = I.make_item_properties x#s_value x#set_value widgets in
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
