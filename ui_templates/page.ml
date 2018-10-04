open Containers
open Components
open Tabs

let main_class = "main-content"
let toolbar_class = "main-toolbar"
let row_id = "main-toolbar__tabs"

class type container =
  object
    inherit Widget.t
    method content : Widget.t option
    method set_content : Widget.t -> unit
  end

type 'a value = string * (unit -> (#Widget.t as 'a))

type ('a, 'b) tab = ('a, 'b value) Tab.t

type ('a,'b) page_content =
  [ `Static  of (#Widget.t as 'b) list
  | `Dynamic of ('a, 'b) tab list
  ]

let ( % ) = Fun.( % )

let hash_of_tab (tab : ('a, 'b) tab) : string =
  fst @@ tab#value

let widget_of_tab (tab : ('a, 'b) tab) : Widget.t =
  Widget.coerce @@ (snd tab#value) ()

let set_hash hash =
  Dom_html.window##.location##.hash := Js.string hash

let switch_tab (s : ('a, 'b) tab option React.signal) =
  React.E.map (set_hash % hash_of_tab)
  @@ React.E.fmap (fun x -> x)
  @@ React.S.changes s

let set_active_page container tab_bar =
  let hash =
    Dom_html.window##.location##.hash
    |> Js.to_string
    |> String.drop 1 in
  let default = List.head_opt tab_bar#scroller#tabs in
  let active : ('a, 'b) tab option =
    List.find_opt (fun (tab : ('a, 'b) tab) ->
        String.equal hash @@ hash_of_tab tab) tab_bar#scroller#tabs
    |> fun x -> Option.choice [ x; default ] in
  begin match active with
  | None -> ()
  | Some tab ->
     container#set_content @@ widget_of_tab tab;
     if not tab#active
     then tab_bar#scroller#set_active_tab tab |> ignore
  end

let create_tab_row (container : container) (tabs : ('a, 'b) tab list) =
  let open Tabs in
  let bar = new Tab_bar.t ~align:Start ~tabs () in
  set_active_page container bar;
  Dom_events.listen Dom_html.window Dom_events.Typ.hashchange (fun _ _ ->
      set_active_page container bar;
      true) |> ignore;
  let section = new Toolbar.Row.Section.t ~align:`Start ~widgets:[bar] () in
  let row = new Toolbar.Row.t ~sections:[section] () in
  let () = row#set_id row_id in
  row, switch_tab bar#scroller#s_active_tab

let get_arbitrary () : container =
  let elt = Dom_html.getElementById "arbitrary-content" in
  object(self)
    val mutable _content = None
    inherit Widget.t elt ()

    method content : Widget.t option = _content
    method set_content (w : Widget.t) =
      begin match _content with
      | Some c -> self#remove_child c; c#destroy ()
      | None -> ()
      end;
      _content <- Some w;
      self#append_child w;
      w#layout ()
  end

let get_toolbar () =
  Dom_html.getElementById "main-toolbar"
  |> Widget.create

class t (content:('a,'b) page_content) () =
  let main = Dom_html.getElementById "main-content" in
  let arbitrary = get_arbitrary () in
  let toolbar = get_toolbar () in
  let title = toolbar#get_child_element_by_id "page-title"
              |> Option.get_exn
              |> Widget.create in
  object(self)

    val mutable _previous_toolbar = None
    val mutable _previous_content = None

    inherit Widget.t main ()

    method title : string =
      Js.to_string Dom_html.document##.title

    method set_title (x : string) : unit =
      Dom_html.document##.title := Js.string x;
      title#set_text_content x

    method set () =
      _previous_content <- Some arbitrary#root##.childNodes;
      arbitrary#set_empty ();
      match content with
      | `Static widgets -> List.iter arbitrary#append_child widgets
      | `Dynamic tabs   ->
         let row, e = create_tab_row arbitrary tabs in
         self#_keep_e e;
         (try
            let elt = Dom_html.getElementById row_id in
            _previous_toolbar <- Some elt;
            Dom.removeChild toolbar#root elt
          with _ -> ());
         self#add_class
         @@ Components_markup.CSS.add_modifier main_class "dynamic";
         toolbar#append_child row

    initializer
      self#add_class main_class;
      toolbar#add_class toolbar_class;
      self#set ()
  end
