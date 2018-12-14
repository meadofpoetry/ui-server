open Containers
open Tyxml_js

module Markup = Components_markup.Select.Make(Xml)(Svg)(Html)

module Item = struct
  class ['a] t ?selected ?disabled ~(value : 'a) ~text () =
    let elt =
      Markup.Item.create ?disabled ~text ()
      |> To_dom.of_option in
    object(self)

      val mutable _value = value

      inherit Widget.t elt () as super

      method! init () : unit =
        super#init ();
        Option.iter self#set_selected selected

      method value : 'a =

        _value
      method set_value (x : 'a) : unit =
        _value <- x

      method text : string =
        Js.to_string self#option_element##.text

      method index : int =
        self#option_element##.index

      method selected : bool =
        Js.to_bool self#option_element##.selected

      method set_selected (x : bool) : unit =
        self#option_element##.selected := Js.bool x

      method disabled : bool =
        Js.to_bool self#option_element##.disabled

      method set_disabled (x : bool) : unit =
        self#option_element##.disabled := Js.bool x

      (* Private methods *)

      method private option_element : Dom_html.optionElement Js.t =
        elt

    end
end

module Group = struct
  class ['a] t ~label ~(items : 'a Item.t list) () =

    let item_elts =
      List.map (fun x -> Of_dom.of_option (Js.Unsafe.coerce x#root))
        items in
    let elt =
      Markup.Item.create_group ~label ~items:item_elts ()
      |> To_dom.of_optgroup in

    object(self)

      val mutable _items = items

      inherit Widget.t elt () as super

      method! destroy () : unit =
        super#destroy ();
        List.iter (fun i ->
            self#remove_child i;
            i#destroy ()) _items;
        _items <- []

      method opt_group_element : Dom_html.optGroupElement Js.t =
        elt

      method items : 'a Item.t list =
        _items

      method label : string =
        Js.to_string self#opt_group_element##.label

      method set_label (s : string) : unit =
        self#opt_group_element##.label := Js.string s

      method disabled : bool =
        Js.to_bool self#opt_group_element##.disabled

      method set_disabled (x : bool) : unit =
        self#opt_group_element##.disabled := Js.bool x

    end
end

class ['a] t ?(disabled = false)
        ?(bottom_line = true)
        ?(default_selected = true)
        ?(label : string option)
        ~(items : [ `Item of 'a Item.t | `Group of 'a Group.t ] list)
        () =
  let make_empty () =
    Markup.Item.create
      ~disabled:true
      ~selected:true
      ~text:""
      () in
  let s, push = React.S.create None in
  let s_value = React.S.map (fun i -> Option.map (fun x -> x#value) i) s in
  let item_elts =
    List.map (function
        | `Group g -> Widget.to_markup g
        | `Item i -> Widget.to_markup i) items
    |> (fun l -> if not default_selected then make_empty () :: l else l) in
  let bottom_line = match bottom_line with
    | false -> None
    | true -> Some (new Line_ripple.t ()) in
  let label = match label with
    | None -> None
    | Some label -> Some (new Floating_label.t label ()) in
  let select =
    Markup.create_select ~items:item_elts ()
    |> To_dom.of_element
    |> Widget.create in
  let elt =
    Markup.create
      ?bottom_line:(Option.map Widget.to_markup bottom_line)
      ?label:(Option.map Widget.to_markup label)
      ~select:(Widget.to_markup select)
      ()
    |> Tyxml_js.To_dom.of_div in
  object(self)
    val mutable _items = items
    inherit Widget.t elt ()

    method select = select
    method bottom_line = bottom_line
    method label = label

    method value : 'a option =
      Option.map (fun (x : 'a Item.t) -> x#value) self#selected_item

    method items : 'a Item.t list =
      List.fold_left (fun acc -> function
          | `Group g -> g#items @ acc
          | `Item i -> i :: acc) [] _items

    method length : int =
      self#_native_select##.length

    method item (n : int) : 'a Item.t option =
      List.get_at_idx n self#items

    method! set_empty () =
      List.iter (function
          | `Group g ->
             self#select#remove_child g;
             g#destroy ()
          | `Item i ->
             self#select#remove_child i;
             i#destroy ()) _items;
      _items <- [];
      push None

    method remove_item (i : 'a Item.t) : unit =
      _items <- List.remove ~eq:(fun a b ->
                    match a, b with
                    | `Item a, `Item b -> Widget.equal a b
                    | _ -> false) (`Item i) _items;
      self#select#remove_child i;
      match self#selected_item with
      | None -> ()
      | Some x ->
         if Widget.equal i x
         then push None

    method append_item (i : 'a Item.t) : unit =
      _items <- `Item i :: _items;
      self#select#append_child i

    method append_group (g : 'a Group.t) : unit =
      _items <- `Group g :: _items;
      self#select#append_child g

    method selected_index : int option =
      self#_native_select##.selectedIndex
      |> fun x -> if x = -1 then None else Some x

    method selected_item : 'a Item.t option =
      Option.flat_map (fun x ->
          List.find_opt (fun i -> i#index = x)
            self#items)
        self#selected_index

    method set_selected_index (i : int) : unit =
      self#_native_select##.selectedIndex := i;
      self#add_class Markup.is_changing_class;
      Dom_html.setTimeout (fun () ->
          self#remove_class Markup.is_changing_class) 125.0
      |> ignore;
      let res = List.find_opt (fun x -> x#index = i) self#items in
      push res

    method set_selected_value ~(eq : 'a -> 'a -> bool)
             (v : 'a) : ('a Item.t, string) result =
      match List.find_opt (fun (x : 'a Item.t) -> eq x#value v) self#items with
      | Some i -> self#set_selected_index i#index; Ok i
      | None -> Error "item not found"

    method s_selected_item : 'a Item.t option React.signal =
      s

    method s_selected_value : 'a option React.signal =
      s_value

    method disabled : bool =
      Js.to_bool self#_native_select##.disabled

    method set_disabled (x : bool) : unit =
      self#add_or_remove_class x Markup.disabled_class;
      self#_native_select##.disabled := Js.bool x

    (* Private methods *)

    method private _native_select : Dom_html.selectElement Js.t =
      Js.Unsafe.coerce select#root

    initializer
      push self#selected_item;
      React.S.map (fun v ->
          Option.iter (fun x -> x#float @@ Option.is_some v) label) self#s_selected_item
      |> self#_keep_s;
      select#listen_lwt Widget.Event.focus (fun _ _ ->
          Option.iter (fun x -> x#activate ()) self#bottom_line;
          Lwt.return_unit) |> Lwt.ignore_result;
      select#listen_lwt Widget.Event.blur (fun _ _ ->
          Option.iter (fun x -> x#deactivate ()) self#bottom_line;
          Lwt.return_unit) |> Lwt.ignore_result;
      select#listen_lwt Widget.Event.change (fun _ _ ->
          Option.iter self#set_selected_index self#selected_index;
          Lwt.return_unit) |> Lwt.ignore_result;
      self#set_disabled disabled

  end


