open Containers

module Cell = struct

  type 'a content = 'a Markup.Table.Cell.content

  class t ~header ~content () =
    let elt = Markup.Table.Cell.create ~header ~content ()
              |> Tyxml_js.To_dom.of_element in

    object

      inherit Widget.widget elt ()

    end

end

module Row = struct

  class t ~(content:'a Cell.content list) () =

    let cells = List.map (fun x -> new Cell.t ~header:false ~content:x ()) content in
    let elt   = Markup.Table.Row.create ~cells:(List.map Widget.widget_to_markup cells) ()
                |> Tyxml_js.To_dom.of_element in

    object

      inherit Widget.widget elt ()

    end

end

module Header = struct

  class t ~(content:'a Cell.content list) () =
    let cells = List.map (fun x -> new Cell.t ~header:true ~content:x ()) content in
    let row   = Markup.Table.Row.create ~cells:(List.map Widget.widget_to_markup cells) () in
    let elt   = Markup.Table.Header.create ~row ()
                |> Tyxml_js.To_dom.of_element in

    object

      inherit Widget.widget elt ()

    end

end

module Body = struct

  class t ~(content:'a Cell.content list list) () =

    let rows = List.map (fun x -> new Row.t ~content:x ()) content in
    let elt  = Markup.Table.Body.create ~rows:(List.map Widget.widget_to_markup rows) ()
               |> Tyxml_js.To_dom.of_element in

    object

      inherit Widget.widget elt ()

    end

end

class t ~header ~(content:'a Cell.content list list) () =

  let body = new Body.t ~content () in
  let tabl = Markup.Table.create_table ~header:(Widget.widget_to_markup header)
               ~body:(Widget.widget_to_markup body)
               () in
  let elt  = Markup.Table.create ~table:tabl ()
             |> Tyxml_js.To_dom.of_element in

  object

    inherit Widget.widget elt ()

  end
