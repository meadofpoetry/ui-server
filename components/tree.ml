open Js_of_ocaml
open Containers
open Tyxml_js

module Markup = Components_markup.Tree.Make(Xml)(Svg)(Html)

module Item = struct

  class ['a, 'b] t ?(expand_on_click = true)
          ?secondary_text
          ?(graphic : #Widget.t option)
          ?(meta : #Widget.t option)
          ?(nested : 'b option)
          ~(value : 'a)
          ~text
          () =

    let s, s_push = React.S.create false in
    let meta = match meta with
      | Some x -> Some x
      | None ->
         Option.map (fun _ ->
             let open Icon.SVG in
             (* FIXME *)
             let path = new Path.t Markup.Path.chevron_down () in
             let icon = new Icon.SVG.t ~paths:[path] () in
             React.S.map (fun x ->
                 if x then path#set Markup.Path.chevron_up
                 else path#set Markup.Path.chevron_down) s |> ignore;
             icon#widget)
           nested in
    let item =
      new Item_list.Item.t ?secondary_text
        ?graphic ?meta ~tag:Html.div ~value ~text () in
    let elt =
      Markup.Item.create ~item:(Widget.to_markup item)
        ?nested_list:(Option.map Widget.to_markup nested) ()
      |> To_dom.of_element in
    object(self)

      val mutable _value = value

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        Option.iter (fun x ->
            x#add_class Markup.Item.list_class;
            item#style##.cursor := Js.string "pointer") nested;
        if expand_on_click then
          self#listen Widget.Event.click (fun _ e ->
              (* FIXME click on a graphic widget
               * opens and closes the tree item,
               * but it shouldn't be *)
              Dom_html.stopPropagation e;
              self#toggle ();
              true)
          |> ignore;
        Option.iter (fun meta ->
            Dom_events.listen meta#root Dom_events.Typ.click
              (fun _ e ->
                Dom_html.stopPropagation e;
                self#toggle ();
                true)
            |> ignore;
          ) meta

      method value : 'a =
        _value

      method set_value (x : 'a) : unit =
        _value <- x

      method item = item

      method nested_tree : 'b option =
        nested

      method expanded : bool =
        self#has_class Markup.Item.item_open_class

      method expand () : unit =
        Option.iter (fun x -> x#add_class Markup.Item.list_open_class)
          nested;
        self#add_class Markup.Item.item_open_class

      method collapse () : unit =
        Option.iter (fun x -> x#remove_class Markup.Item.list_open_class)
          nested;
        self#remove_class Markup.Item.item_open_class

      method toggle () : unit =
        let open_class = Markup.Item.item_open_class in
        let open_list  = Markup.Item.list_open_class in
        Option.iter (fun x ->
            x#toggle_class open_list
            |> ignore)
          self#nested_tree;
        self#toggle_class open_class |> s_push

    end

end

type selection =
  [ `Single
  | `Multiple
  ]

class ['a] t
        ?(selection : selection option)
        ?level
        ?two_line
        ?(dense = false)
        ~(items : ('a, 'a t) Item.t list)
        () =
  let two_line = match two_line with
    | Some x -> x
    | None ->
       List.find_pred (fun x -> Option.is_some x#item#secondary_text) items
       |> Option.is_some in
  let elt = Markup.create ~two_line ~items:(List.map Widget.to_markup items) ()
            |> To_dom.of_element in
  let s_selected, set_selected = React.S.create [] in
  let s_active, set_active = React.S.create None in
  object(self)

    val mutable _items = items

    inherit Widget.t elt () as super

    method! init () : unit =
      super#init ();
      self#set_dense dense;
      match level with
      | None -> self#_padding ()
      | Some l -> self#set_attribute "data-level" @@ string_of_int l

    method items = _items

    method active : ('a, 'a t) Item.t option =
      React.S.value s_active

    method s_active : ('a, 'a t) Item.t option React.signal =
      s_active

    method set_active (item : ('a, 'a t) Item.t) =
      Option.iter (fun x ->
          x#item#remove_class Markup.Item_list.Item.activated_class) self#active;
      item#item#add_class Markup.Item_list.Item.activated_class;
      set_active (Some item)

    method selected : ('a, 'a t) Item.t list =
      React.S.value s_selected
    method s_selected : ('a, 'a t) Item.t list React.signal =
      s_selected
    method set_selected (item : ('a, 'a t) Item.t) =
      match selection with
      | None -> ()
      | Some `Single ->
         List.iter (fun i ->
             i#item#remove_class Markup.Item_list.Item.selected_class) self#selected;
         item#item#add_class Markup.Item_list.Item.selected_class;
         set_selected [item]
      | Some `Multiple ->
         item#item#add_class Markup.Item_list.Item.selected_class;
         set_selected @@ item :: self#selected

    method dense : bool =
      self#has_class Markup.dense_class
    method set_dense (x : bool) : unit =
      self#add_or_remove_class x Markup.dense_class;
      self#iter (fun (i : ('a, 'a t) Item.t) ->
          Option.iter (fun (t : 'a t) -> t#set_dense x)
            i#nested_tree)

    method append_item (x : ('a, 'a t) Item.t) =
      _items <- _items @ [x];
      self#append_child x

    method private iter f =
      let rec iter l =
        List.iter (fun (x : ('a, 'a t) Item.t) ->
            f x;
            match x#nested_tree with
            | None -> ()
            | Some n -> iter n#items) l in
      iter self#items

    method private _padding () =
      let rec iter l n =
        List.iter (fun x ->
            let item = (Js.Unsafe.coerce x#root)##querySelector (Js.string ".mdc-list-item") in
            item##.style##.paddingLeft := Js.string @@ (string_of_int (n*16))^"px";
            match x#nested_tree with
            | None -> ()
            | Some el -> iter el#items (n+1)) l in
      iter self#items 1

  end

(** Create new widget from scratch *)
let make ?(selection : selection option) ?level ?two_line ?dense
      ~(items : ('a, 'a t) Item.t list) () : 'a t =
  new t ?selection ?level ?two_line ?dense ~items ()

(* let attach (elt : Dom_html.element Js.t) : 'a t =
 *   failwith "" *)
