open Containers
open Components

let base_class = "wm-selectable-title"

class title ~title ~widget () =
  let title_class = CSS.add_element base_class  "title" in
  let active_class = CSS.add_modifier title_class "active" in
  object

    inherit Typography.Text.t
              ~adjust_margin:false
              ~text:title
              ~font:Subtitle_2
              () as super

    method! init () : unit =
      super#init ();
      super#add_class title_class

    method set_active (x : bool) : unit =
      super#toggle_class ~force:x active_class;
      widget#style##.display := Js_of_ocaml.Js.string (if x then "" else "none")

    method get_title : string = title

  end

class t titles () =
  let (titles : title list) =
    List.map (fun (title, widget) ->
        new title ~title ~widget ()) titles in
  object(self)

    inherit Hbox.t ~widgets:titles () as super

    method! init () : unit =
      super#init ();
      super#add_class base_class;
      let _ =
        List.map (fun (w : #Widget.t) ->
            w#listen_click_lwt (fun _ _ -> self#select w; Lwt.return_unit))
          self#titles in
      match self#titles with
      | [] -> failwith "Titles must not be empty"
      | x :: _ -> self#select x

    method titles : title list = titles

    method select (w : title) : unit =
      List.iter (fun (x : title) ->
          if not @@ Widget.equal x w
          then x#set_active false) titles;
      w#set_active true

    method select_by_name (n : string) : unit =
      match List.find_opt (fun x -> String.equal x#get_title n) self#titles with
      | None -> ()
      | Some x -> self#select x

  end

let make titles = new t titles ()
