open Containers
open Components

let base_class = "wm-selectable-title"

class title ~title ~widget () =
  let title_class  = Markup.CSS.add_element  base_class  "title"  in
  let active_class = Markup.CSS.add_modifier title_class "active" in
  object(self)

    inherit Typography.Text.t ~adjust_margin:false ~text:title ~font:Subheading_2 ()

    method set_active x =
      self#add_or_remove_class x active_class;
      widget#style##.display := Js.string (if x then "" else "none")

    method get_title = title

    initializer
      self#add_class title_class

  end

class t titles () =
  let (titles : title list) =
    List.map (fun (title,widget) -> new title ~title ~widget ()) titles in
  object(self)

    inherit Box.t ~vertical:false ~widgets:titles ()

    method titles : title list = titles
    method select (w:title) =
      List.iter (fun x -> if not @@ Equal.physical x#root w#root then x#set_active false) titles;
      w#set_active true
    method select_by_name n =
      match List.find_opt (fun x -> String.equal x#get_title n) self#titles with
      | None   -> ()
      | Some x -> self#select x

    initializer
      self#add_class base_class;
      let open Dom_events in
      let _ = List.map (fun w -> listen w#root Typ.click (fun _ _ -> self#select w; false)) self#titles in
      match self#titles with
      | []           -> failwith "Titles must not be empty"
      | [x] | x :: _ -> self#select x

  end

let make titles = new t titles ()