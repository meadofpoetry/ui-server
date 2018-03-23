open Containers
open Components

let make_resolution_input ?range ~label () =
  new Textfield.t ~label ~input_type:(Integer range) ()

class t () =
  let _class = "wm-resolution-dialog" in
  let width  = make_resolution_input ~label:"Ширина" () in
  let height = make_resolution_input ~label:"Высота" () in
  let accept = new Dialog.Action.t ~typ:`Accept  ~label:"Ok" () in
  let cancel = new Dialog.Action.t ~typ:`Decline ~label:"Отмена" () in
  let widget = new Box.t ~vertical:true ~widgets:[width;height] () in

  object(self)

    inherit Dialog.t ~title:"Установка разрешения"
                     ~actions:[cancel;accept]
                     ~content:(`Widgets [widget])
                     ()

    method show_await_resolution init =
      let open Lwt.Infix in
      width#fill_in (fst init); height#fill_in (snd init);
      self#show_await
      >>= (function
           | `Accept -> let w = Option.get_exn @@ React.S.value width#s_input in
                        let h = Option.get_exn @@ React.S.value height#s_input in
                        Lwt.return_some (w,h)
           | `Cancel -> Lwt.return_none)


    initializer
      self#add_class _class;
      React.S.l2 (fun w h ->
          match w,h with
          | Some _, Some _ -> accept#set_disabled false
          | _              -> accept#set_disabled true)
                 width#s_input height#s_input
      |> ignore

  end

let make () =
  new t ()
