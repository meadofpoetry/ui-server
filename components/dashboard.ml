open Containers

include Dashboard_common
include Dashboard_grid

module Position = Dynamic_grid.Position
module Item = Dashboard_item

let ( % ) = Fun.( % )

type serialized = Yojson.Safe.json

type edit =
  { add : bool
  ; remove : bool
  }

type edit_caps =
  | Absolute
  | Partial of edit
  | Forbidden

type 'a content =
  | Serialized of Yojson.Safe.json
  | Items of 'a Item.positioned_item list

type load_error =
  | Add_error of Dynamic_grid_abstract.add_error
  | Bad_format of string

let sort l =
  Position.sort_by_y ~f:(fun (x : 'a Item.positioned_item) -> x.position) l

class ['a] t ?(edit_caps = Absolute)
        ?on_edit
        ?(default : 'a content option)
        ?(init : 'a content option)
        (factory : 'a #factory) () =
  let grid = new grid factory () in
  let add_panel =
    new Dashboard_panel.add
      ~widgets:(List.map (fun x -> new Dashboard_add_item.t x ())
                  (match factory#available with
                   | `List l -> l
                   | `Groups _ -> [])) () in
  let edit_icon = Icon.SVG.(create_simple Path.pencil) in
  let add_icon = Icon.SVG.(create_simple Path.plus) in
  let add = new Fab.t ~icon:add_icon () in
  let fab =
    new Fab_speed_dial.t
      ~direction:`Up
      ~animation:`Scale
      ~icon:edit_icon
      ~items:[add]
      () in
  let e, push = React.E.create () in
  object(self)

    val mutable _add_listener = None
    val mutable _restore_listener = None
    val mutable _restore_fab = None

    inherit Vbox.t ~widgets:[grid#widget; fab#widget] () as super

    method! init () : unit =
      super#init ();
      self#set_edit_caps edit_caps;
      let add_listener =
        add#listen_click_lwt (fun _ _ ->
            add_panel#show_await ()) in
      _add_listener <- Some add_listener;
      fab#main#listen Widget.Event.click (fun _ _ ->
          (match React.S.value fab#s_state with
           | false -> fab#show ()
           | true ->
              push @@ self#serialize ();
              Option.iter (fun f -> f @@ self#serialize ()) on_edit;
              fab#hide ());
          true)|> ignore;
      React.S.map (function
          | true  -> grid#set_editable true;
                     edit_icon#path#set Icon.SVG.Path.check
          | false -> grid#set_editable false;
                     edit_icon#path#set Icon.SVG.Path.pencil)
        fab#s_state |> ignore;
      Dom.appendChild Dom_html.document##.body add_panel#root;
      self#set_on_load @@ Some (fun () -> self#grid#layout (); fab#hide ());
      fab#add_class Markup.edit_button_class;
      self#add_class Markup.base_class;
      self#grid#set_editable false;
      (* Add 'restore' action if default layout was provided *)
      begin match default with
      | None -> ()
      | Some init ->
         let icon = Icon.SVG.(create_simple Path.restore) in
         let restore = new Fab.t ~icon () in
         let listener =
           restore#listen_click_lwt (fun _ _ ->
               ignore @@ self#load init;
               Lwt.return_unit) in
         _restore_listener <- Some listener;
         fab#append restore
      end;
      (* Load content, if any *)
      begin match init, default with
      | Some init, _ | None, Some init -> ignore @@ self#load init
      | None, None -> ()
      end

    method! destroy () : unit =
      super#destroy ();
      self#grid#destroy ();
      factory#destroy ();
      Option.iter (fun x -> x#destroy ()) _restore_fab;
      _restore_fab <- None;
      Option.iter Lwt.cancel _restore_listener;
      _restore_listener <- None;
      Option.iter Lwt.cancel _add_listener;
      _add_listener <- None;
      Dom.removeChild Dom_html.document##.body add_panel#root

    method e_edited : Yojson.Safe.json React.event = e

    method grid = grid

    method set_edit_caps : edit_caps -> unit = function
      | Absolute -> self#set_absolute_edit_caps ()
      | Partial caps -> self#set_partial_edit_caps caps
      | Forbidden -> self#add_class Markup.non_editable_class

    method positioned_items : 'a Item.positioned_item list =
      List.map (fun x ->
          ({ position = x#pos
           ; item = fst x#value } : 'a Item.positioned_item))
        self#grid#items

    method load : 'a content -> (unit, load_error) result = function
      | Serialized x -> self#load_serialized x
      | Items x -> self#load_raw x

    method serialize () : serialized =
      let f = Item.positioned_item_to_yojson factory#serialize in
      `List (List.map f self#positioned_items)

    (* Private methods *)

    method private load_raw (items : 'a Item.positioned_item list) =
      let prev_items = self#positioned_items in
      self#grid#remove_all ();
      match List.all_ok @@ List.map self#grid#add @@ sort items with
      | Ok _ -> Ok ()
      | Error e ->
         List.iter (ignore % self#grid#add) @@ sort prev_items;
         Error (Add_error e)

    method private load_serialized (data : serialized)
                   : (unit, load_error) result =
      match self#deserialize data with
      | Error e -> Error (Bad_format e)
      | Ok x -> self#load_raw x

    method private deserialize (serialized : serialized)
                   : ('a Item.positioned_item list, string) result =
      match serialized with
      | `List l ->
         let f = Item.positioned_item_of_yojson factory#deserialize in
         List.all_ok @@ List.map f l
      | _ -> Error "not a list"

    method private set_absolute_edit_caps () : unit =
      add#style##.display := Js.string "";
      self#remove_class Markup.non_editable_class

    method private set_partial_edit_caps (caps : edit) : unit =
      if not caps.add
      then add#style##.display := Js.string "none"
      else add#style##.display := Js.string "";
      self#remove_class Markup.non_editable_class

  end
