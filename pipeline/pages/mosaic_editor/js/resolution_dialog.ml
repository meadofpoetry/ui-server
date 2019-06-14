open Js_of_ocaml
open Components

let make_resolution_input ?range ~id ~label () =
  let range = match range with
    | None -> None, None
    | Some (x, y) -> Some x, Some y
  in
  Textfield.make_textfield
    ~label
    ~input_id:id
    (Integer range)

class t () =
  let _class = "wm-resolution-dialog" in
  let width = make_resolution_input ~id:"wm-width"  ~label:"Ширина" () in
  let height = make_resolution_input ~id:"wm-height" ~label:"Высота" () in
  let widget = Box.make ~dir:`Column [width; height] in
  let title = Dialog.Markup.create_title_simple ~title:"Установка разрешения" () in
  let content = Dialog.Markup.create_content
      ~content:[Widget.to_markup widget]
      () in
  let actions =
    [ Dialog.Markup.create_action
        ~action:Accept
        ~label:"ОК"
        ()
    ; Dialog.Markup.create_action
        ~action:Close
        ~label:"Отмена"
        () ] in
  let elt = Dialog.make_element ~title ~actions ~content () in
  object

    inherit Dialog.t elt () as super

    val mutable _s = None

    method! init () : unit =
      super#init ();
      super#add_class _class

    method! destroy () : unit =
      super#destroy ();
      Utils.Option.iter (React.S.stop ~strong:true) _s;
      _s <- None

    method show_await_resolution init =
      let open Lwt.Infix in
      width#set_value (fst init); height#set_value (snd init);
      super#open_await ()
      >>= function
      | Close | Destroy | Custom _  -> Lwt.return_none
      | Accept ->
        match width#value, height#value with
        | None, _ | _, None -> Lwt.return_none
        | Some w, Some h -> Lwt.return_some (w, h)
  end

let make () = new t ()
