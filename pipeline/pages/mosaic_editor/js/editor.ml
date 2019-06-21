open Js_of_ocaml
open Components
open Pipeline_types

(* TODO
   1. Add placeholder when the mosaic is empty (both for containers & widgets) *)

type event =
  [ `Layout of Wm.t
  ]

let ( % ) f g x = f (g x)

(* TODO remove later *)
module Test = struct

  let make_widget ?(type_ = Wm.Video) ?aspect ~x ~y ~w ~h () : string * Wm.widget =
    let position =
      Some { Wm.
             left = x
           ; top = y
           ; right = x + w
           ; bottom = y + h
           } in
    string_of_int @@ Random.bits (),
    { position
    ; description = "Sample widget"
    ; pid = Some 4096
    ; type_
    ; aspect
    ; domain = Nihil
    ; layer = 0
    }
  let make_container ?(widgets = []) ~position () : string * Wm.container =
    "Sample container", { position
                        ; widgets
                        }
  let container =
    make_container
      ~position:{ left = 0; top = 0; right = 1920; bottom = 1080 }
      ~widgets:[ make_widget ~x:0 ~y:0 ~w:111 ~h:150 ()
               ; make_widget ~x:111 ~y:0 ~w:189 ~h:150 ()
               ; make_widget ~x:0 ~y:150 ~w:200 ~h:150 ()
               ; make_widget ~x:210 ~y:150 ~w:90 ~h:150 ~type_:Audio ()
               ]
      ()

end

(** Switches top app bar between contextual action
    mode and normal mode *)
let transform_top_app_bar
    ?(actions = [])
    ~(title : string)
    ~(class_ : string)
    (scaffold : Scaffold.t)
    (x : Top_app_bar.t) =
  let prev_title = x#title in
  let prev_actions = x#actions in
  x#set_title title;
  x#add_class class_;
  x#set_actions @@ List.map Widget.root actions;
  (fun () ->
     scaffold#set_on_navigation_icon_click_default ();
     List.iter Widget.destroy actions;
     x#set_title prev_title;
     x#set_actions prev_actions;
     x#remove_class class_)

let handle_item_selected
    (scaffold : Scaffold.t)
    item =
  match scaffold#top_app_bar with
  | None -> fun () -> ()
  | Some top_app_bar ->
    (* let remove =
     * Actions.make_action
     *     ~on_click:(fun _ _ -> item#remove (); Lwt.return_unit)
     *     { icon = Icon.SVG.(make_simple Path.delete)#widget
     *     ; name = "Удалить"
     *     } in
     * let edit =
     *   Actions.make_action
     *     ~on_click:(fun _ _ -> Lwt.return_unit)
     *     { icon = Icon.SVG.(make_simple Path.pencil)#widget
     *     ; name = "Редактировать"
     *     } in *)
    transform_top_app_bar
      ~class_:Page_mosaic_editor_tyxml.CSS.top_app_bar_contextual
      ~actions:[] (* [edit; remove] *)
      ~title:"TODO"
      scaffold
      top_app_bar

class t ~(layout: Wm.t)
    (elt : Dom_html.element Js.t)
    (scaffold : Scaffold.t)
    () = object(self)

  val mutable _widget_editor = None
  val mutable _listeners = []
  val mutable _resize_observer = None

  inherit Widget.t elt () as super

  method! init () : unit =
    _resize_observer <- Some (
        Ui_templates.Resize_observer.observe
          ~f:(fun _ -> self#layout ())
          ~node:super#root
          ());
    super#init ()

  method! initial_sync_with_dom () : unit =
    _listeners <- Events.(
        [ listen_lwt super#root Resizable.Event.selected self#handle_widget_selected
        ]);
    super#initial_sync_with_dom ()

  method! destroy () : unit =
    Utils.Option.iter Ui_templates.Resize_observer.disconnect _resize_observer;
    _resize_observer <- None;
    List.iter Lwt.cancel _listeners;
    _listeners <- [];
    super#destroy ()

  method! layout () : unit =
    Utils.Option.iter (Widget.layout % snd) _widget_editor;
    super#layout ()

  method notify : event -> unit = function
    | `Layout wm ->
      (match _widget_editor with
       | None -> ()
       | Some (id, editor) ->
         match List.assoc_opt id wm.layout with
         | None -> () (* FIXME container lost, handle it somehow *)
         | Some x -> editor#notify @@ `Container x)

  (* Private methods *)

  method switch_state (id, container : string * Wm.container) : unit =
    let widget_editor = Widget_editor.make container in
    _widget_editor <- Some (id, widget_editor);
    (* FIXME for test purposes *)
    let add_toolbar_actions (scaffold : Scaffold.t) =
      match scaffold#top_app_bar with
      | None -> ()
      | Some (top_app_bar : Top_app_bar.t) ->
        let selector = ".mdc-top-app-bar__row:nth-child(2)" in
        match Element.query_selector top_app_bar#root selector with
        | None -> ()
        | Some row ->
          Element.append_child row widget_editor#toolbar#root in
    add_toolbar_actions scaffold;
    super#append_child widget_editor;
    ()

  method private handle_widget_selected e _ =
    ignore @@ Js.Unsafe.global##.console##log e;
    Lwt.return_unit
end

let make layout scaffold =
  let elt = Dom_html.createDiv Dom_html.document in
  Element.add_class elt "editor";
  let t = new t ~layout elt scaffold () in
  t#switch_state Test.container;
  t
