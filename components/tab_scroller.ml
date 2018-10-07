open Containers
open Tyxml_js

module Markup = Components_markup.Tab_scroller.Make(Xml)(Svg)(Html)

type align =
  | Start
  | End
  | Center

let horizontal_scroll_height = ref None

let compute_horizontal_scroll_height ?(cache = true) () : int option =
  match cache, !horizontal_scroll_height with
  | true, Some x -> Some x
  | _ ->
     let el = Dom_html.(createDiv document) in
     el##.classList##add (Js.string Markup.scroll_test_class);
     Dom.appendChild Dom_html.document##.body el;
     let height = el##.offsetHeight - el##.clientHeight in
     if cache then horizontal_scroll_height := Some height;
     Dom.removeChild Dom_html.document##.body el;
     Some height

class ['a, 'b] t ?on_change ?align
        ~(tabs:('a, 'b) Tab.t list) () =
  let eq = Widget.equal in
  let tabs' = List.map Widget.to_markup tabs in
  let content =
    Markup.create_scroll_content tabs' () in
  let area =
    Markup.create_scroll_area ~content ()
    |> To_dom.of_element
    |> Widget.create in
  let elt =
    Markup.create ~scroll_area:(Widget.to_markup area) ()
    |> To_dom.of_element in
  let s_active, set_active = React.S.create ~eq:(Equal.option eq) None in
  object(self)

    val mutable _tabs : ('a, 'b) Tab.t list = tabs
    val mutable _align : align option = align

    inherit Widget.t elt ()

    method s_active_tab : ('a, 'b) Tab.t option React.signal =
      s_active

    method active_tab : ('a, 'b) Tab.t option =
      React.S.value self#s_active_tab

    method active_tab_index : int option =
      match self#active_tab with
      | None -> None
      | Some x -> List.find_idx (eq x) self#tabs
                  |> Option.map fst

    method active_tab_value : 'b option =
      Option.map (fun x -> x#value) @@ self#active_tab

    method set_active_tab (tab : ('a, 'b) Tab.t) : (unit, string) result =
      match List.find_opt (eq tab) self#tabs with
      | None -> Error "tab not found"
      | Some tab ->
         tab#set_active ?previous:self#active_tab true;
         set_active @@ Some tab;
         self#scroll_tab_into_view tab;
         self#layout ();
         Ok ()

    method set_active_tab_index (i : int) : (unit, string) result =
      match List.get_at_idx i self#tabs with
      | None -> Error "tab not found"
      | Some tab ->
         tab#set_active ?previous:self#active_tab true;
         set_active @@ Some tab;
         self#scroll_tab_into_view tab;
         self#layout ();
         Ok ()

    method append_tab (tab : ('a, 'b) Tab.t) : unit =
      _tabs <- _tabs @ [tab];
      self#append_child tab;
      self#_listen_click tab;
      self#layout ()

    method insert_tab_at_idx (i : int) (tab : ('a,'b) Tab.t) : unit =
      _tabs <- List.insert_at_idx i tab _tabs;
      self#insert_child_at_idx i tab;
      self#_listen_click tab;
      self#layout ()

    method remove_tab (tab : ('a, 'b) Tab.t) : unit =
      match List.find_opt (eq tab) self#tabs with
      | None -> ()
      | Some tab ->
         self#remove_child tab;
         _tabs <- List.remove ~eq ~x:tab self#tabs;
         if tab#active
         then self#set_active_tab_index 0 |> ignore;
         self#layout ()

    method remove_tab_at_idx (i : int) : unit =
      match List.get_at_idx i self#tabs with
      | None -> ()
      | Some tab ->
         _tabs <- List.remove_at_idx i self#tabs;
         self#remove_child tab;
         if tab#active
         then self#set_active_tab_index 0 |> ignore;
         self#layout ()

    method align : align option = _align
    method set_align (x : align option) : unit =
      _align <- x;
      let pre = Components_markup.CSS.add_modifier
                  Markup.base_class "align-" in
      List.iter self#remove_class @@ self#find_classes pre;
      match x with
      | None -> ()
      | Some Start -> self#add_class Markup.align_start_class
      | Some End -> self#add_class Markup.align_end_class
      | Some Center -> self#add_class Markup.align_center_class

    method tabs = _tabs

    (* Private methods *)

    method private scroll_tab_into_view (tab : ('a, 'b) Tab.t) =
      let left = area#scroll_left in
      let width = area#client_width in
      let right = left + width in
      if tab#left < left
      then self#scroll tab#left
      else if tab#left + tab#width > right
      then self#scroll @@ left + (tab#width + tab#left - right)

    method private scroll next =
      let old = area#scroll_left in
      Utils.Animation.animate
        ~timing:Utils.Animation.Timing.in_out_sine
        ~draw:(fun x ->
          let n = float_of_int next in
          let o = float_of_int old in
          let v = int_of_float @@ (x *. (n -. o)) +. o in
          area#set_scroll_left v)
        ~duration:0.35

    method private _listen_click (tab : ('a, 'b) Tab.t) =
      Lwt_js_events.clicks tab#root (fun _ _ ->
          if not @@ (Equal.option eq) (Some tab) self#active_tab
          then self#set_active_tab tab |> ignore;
          Lwt.return_unit) |> Lwt.ignore_result

    initializer
      area#style##.marginBottom :=
        (match compute_horizontal_scroll_height () with
         | Some x -> Js.string (Printf.sprintf "-%dpx" x)
         | None -> Js.string "");
      area#add_class Markup.scroll_area_scroll_class;
      React.E.map (Option.iter
                     (fun x -> x#set_active ?previous:self#active_tab false))
      @@ React.S.diff (fun _ o -> o) self#s_active_tab |> self#_keep_e;
      (match self#tabs with
       | hd :: _ -> self#set_active_tab hd |> ignore
       | _ -> ());
      List.iter self#_listen_click self#tabs;
      self#set_align _align

  end
