open Containers
open Tyxml_js

module Markup = Components_markup.Tab_scroller.Make(Xml)(Svg)(Html)

type align =
  | Start
  | End
  | Center

let eq x y =
  Equal.physical x#root y#root

class ['a,'b] t ?on_change ?align
        ~(tabs:('a,'b) Tab.t list) () =
  let tabs'   = List.map Widget.to_markup tabs in
  let content = Markup.create_scroll_content tabs' () in
  let area    = Markup.create_scroll_area ~content () in
  let elt     = Markup.create ~scroll_area:area ()
                |> To_dom.of_element in
  let s_active, set_active = React.S.create ~eq:(Equal.option eq) None in
  object(self)

    val mutable _tabs  : ('a,'b) Tab.t list = tabs
    val mutable _align : align option = align

    inherit Widget.t elt ()

    method s_active_tab : ('a,'b) Tab.t option React.signal =
      s_active

    method active_tab : ('a,'b) Tab.t option =
      React.S.value self#s_active_tab
    method active_tab_index : int option =
      match self#active_tab with
      | Some x -> List.find_idx (eq x) self#tabs
                  |> Option.map fst
      | None   -> None
    method active_tab_value : 'b option =
      Option.map (fun x -> x#value) @@ self#active_tab

    method set_active_tab (tab:('a,'b) Tab.t) : (unit, string) result =
      match List.find_opt (eq tab) self#tabs with
      | Some tab ->
         tab#set_active ?previous:self#active_tab true;
         set_active @@ Some tab;
         self#layout ();
         Ok ()
      | None     -> Error "tab not found"

    method set_active_tab_index (i:int) : (unit, string) result =
      match List.get_at_idx i self#tabs with
      | Some tab ->
         tab#set_active ?previous:self#active_tab true;
         set_active @@ Some tab;
         self#layout ();
         Ok ()
      | None     -> Error "tab not found"

    method append_tab (tab:('a,'b) Tab.t) : unit =
      _tabs <- _tabs @ [tab];
      self#append_child tab;
      self#_listen_click tab;
      self#layout ()

    method insert_tab_at_idx (i:int) (tab:('a,'b) Tab.t) : unit =
      _tabs <- List.insert_at_idx i tab _tabs;
      self#insert_child_at_idx i tab;
      self#_listen_click tab;
      self#layout ()

    method remove_tab (tab:('a,'b) Tab.t) : unit =
      match List.find_opt (eq tab) self#tabs with
      | Some tab ->
         self#remove_child tab;
         _tabs <- List.remove ~eq ~x:tab self#tabs;
         if tab#active
         then self#set_active_tab_index 0 |> ignore;
         self#layout ()
      | None -> ()

    method remove_tab_at_idx (i:int) : unit =
      match List.get_at_idx i self#tabs with
      | Some tab ->
         _tabs <- List.remove_at_idx i self#tabs;
         self#remove_child tab;
         if tab#active
         then self#set_active_tab_index 0 |> ignore;
         self#layout ()
      | None -> ()

    method align : align option = _align
    method set_align (x:align option) : unit =
      _align <- x;
      let pre = Components_markup.CSS.add_modifier
                  Markup.base_class "align-" in
      List.iter self#remove_class @@ self#find_classes pre;
      match x with
      | None -> ()
      | Some Start  -> self#add_class Markup.align_start_class
      | Some End    -> self#add_class Markup.align_end_class
      | Some Center -> self#add_class Markup.align_center_class

    method tabs = _tabs

    method private _listen_click (tab:('a,'b) Tab.t) =
      Lwt_js_events.clicks tab#root (fun _ _ ->
          if not @@ (Equal.option eq) (Some tab) self#active_tab
          then self#set_active_tab tab |> ignore;
          Lwt.return_unit) |> Lwt.ignore_result

    initializer
      React.E.map (Option.iter
                     (fun x -> x#set_active ?previous:self#active_tab false))
      @@ React.S.diff (fun n o -> o) self#s_active_tab |> self#_keep_e;
      (match self#tabs with
       | hd :: _ -> self#set_active_tab hd |> ignore
       | _       -> ());
      List.iter self#_listen_click self#tabs;
      self#set_align _align

  end
