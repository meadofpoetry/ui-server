open Containers
open Components

let make_resolution_input ?range ~id ~label () =
  let range = match range with
    | None -> None, None
    | Some (x, y) -> Some x, Some y
  in
  new Textfield.t ~label ~input_id:id ~input_type:(Integer range) ()

class t () =
  let _class = "wm-resolution-dialog" in
  let width = make_resolution_input ~id:"wm-width"  ~label:"Ширина" () in
  let height = make_resolution_input ~id:"wm-height" ~label:"Высота" () in
  let accept = new Button.t ~label:"Ok" () in
  let cancel = new Button.t ~label:"Отмена" () in
  let widget = new Vbox.t ~widgets:[width;height] () in

  object

    inherit Dialog.t
              ~title:"Установка разрешения"
              ~actions:[ Dialog.Action.make ~typ:`Cancel cancel
                       ; Dialog.Action.make ~typ:`Cancel accept ]
              ~content:(`Widgets [widget])
              () as super

    val mutable _s = None

    method! init () : unit =
      super#init ();
      super#add_class _class;
      let s = React.S.l2 (fun w h ->
                  match w,h with
                  | Some _, Some _ -> accept#set_disabled false
                  | _ -> accept#set_disabled true)
                width#s_input height#s_input in
      _s <- Some s

    method! destroy () : unit =
      super#destroy ();
      Option.iter (React.S.stop ~strong:true) _s;
      _s <- None

    method show_await_resolution init =
      let open Lwt.Infix in
      width#set_value (fst init); height#set_value (snd init);
      super#show_await ()
      >>= (function
           | `Cancel -> Lwt.return_none
           | `Accept ->
              let w = Option.get_exn @@ React.S.value width#s_input in
              let h = Option.get_exn @@ React.S.value height#s_input in
              Lwt.return_some (w, h))
  end

let make () =
  new t ()
