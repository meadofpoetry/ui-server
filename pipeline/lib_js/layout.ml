open React
open Unix

let cols = 12
let lines = 12
let vc = true                            (*vc stands for Vertical Compacting. change it to false to turn off the vertical compacting*)

let hor = ((Js.Optdef.to_option @@ Dom_html.window##.innerWidth
          |> CCOpt.get_or ~default:0)-10)/cols

let () = Printf.printf "OK hor %d\n" hor;;

let vert = ((Js.Optdef.to_option @@ Dom_html.window##.innerHeight
           |> CCOpt.get_or ~default:0)-10)/lines

let ()= Printf.printf "OK vert %d\n" vert;;

let x_bounds = 12
let y_bounds = 12

let js_string_px x =                                                   (*the function is used to turn int value into an appropriate for js_of_ocaml html objects format*)
  Js.string (string_of_int x^"px");;

let round x =                                                          (*the function round does round a number the right way*)
  if (x < (floor (x) +. 0.5))
  then int_of_float(floor (x))
  else int_of_float(ceil (x));;

module Layout = struct

module Style = struct                                                  (*here we go to define most of html style declarations via our small and beautiful module*)
  type t = Dom_html.divElement
  let title t s =
    t##.title := Js.string s
  let width t w =
    t##.style##.width := js_string_px w
  let height t h =
    t##.style##.height := js_string_px h
  let left t x =
    t##.style##.left := js_string_px x
  let top t y =
    t##.style##.top := js_string_px y
  let z_index t s =
    t##.style##.zIndex := Js.string s
  let min_width t x =
    t##.style##.minWidth := js_string_px x
  let min_height t y =
    t##.style##.minHeight := js_string_px y
  let max_width t x =
    t##.style##.maxWidth := js_string_px x
  let max_height t y =
    t##.style##.maxHeight := js_string_px y
  let font_size t i =
    t##.style##.fontSize := Js.string (string_of_int i)
  let font_style t s =
    t##.style##.fontStyle := Js.string s
  let class_name t s =
    t##.className := Js.string s
  let text_content t s =
    t##.textContent := Js.Opt.return (Js.string s)
end

module Element = struct
  type t = { mutable width     : int
           ; mutable height    : int
           ; mutable static    : bool
           ; mutable x         : int
           ; mutable y         : int
           ; element           : Dom_html.divElement Js.t
           ; obj               : Dom_html.divElement Js.t
           }

  let create x1 y1 w h s obj1=
    {width = w; height = h; static = s; x = x1; y = y1; element = Dom_html.createDiv Dom_html.document; obj = obj1}

  let set_x el x1 = {el with x = x1}

  let set_y el y1 = {el with y = y1}

  let set_width el w = {el with width = w}

  let set_height el h = {el with height = h}

  let set_static el s = {el with static = s}

  let create_ghost x y w h z =                                       (*the function creates and returns an element object used to show a user the future pos. and size of element*)
    let gh = Dom_html.createDiv Dom_html.document in
    Style.class_name gh "ghost";
    Style.left gh (hor*x);
    Style.width gh (hor*w-10);
    Style.height gh (vert*h-10);
    Style.top gh (vert*y);
    Style.z_index gh z;
    Style.max_width gh (hor*10-10);
    Style.max_height gh (vert*10-10);
    gh

  let create_button () =                                             (*the function creates and returns a button for resizing an element and a visual part of it*)
    let button = Dom_html.createDiv Dom_html.document in
    let visual = Dom_html.createDiv Dom_html.document in
    Style.class_name button "button";
    Style.class_name visual "visual";
    button,visual

  let draw_element el =                                              (*the function is used to draw visually an html object 'element'*)
    Style.class_name el.element "element";
    Style.left el.element (hor*el.x);
    Style.width el.element (hor*el.width-10);                        (*minus ten in width and height means the blank space between the elements*)
    Style.height el.element (vert*el.height-10);
    Style.top el.element (vert*el.y);
    Style.min_width el.element (hor-10);
    Style.min_height el.element (vert-10);
    Style.max_width el.element (hor*10-10);
    Style.max_height el.element (vert*10-10)

  let static el =                                                    (*the function is used to make an html element static if it has such parameter*)
    Style.font_size el.element 15;
    Style.font_style el.element "italic";
    Style.text_content el.element "Static"

  let compare1 l1 l2 =                                               (*the function compare1 is used in Array.sort. it works like a st. compare ocaml function, but for layout_el objects*)
    if l1.y = l2.y then 0
    else if l1.y > l2.y then 1
    else -1

  let collides l1 l2 =                                               (*the function collides checks if two elements collide, returns true if do and false otherwise*)
    if (l1 == l2) then false
    else if (l1.x + l1.width <= l2.x) then false
    else if (l1.x >= l2.x + l2.width) then false
    else if (l1.y + l1.height <= l2.y) then false
    else if (l1.y >= l2.y + l2.height) then false
    else true

  let correct_bounds el =                                                 (*the function correct_bounds checks if the elements are within horizontal bounds*)
    if ((el.x + el.width)>= x_bounds) then (el.x <- (x_bounds - el.width))
    else if (el.x < 0) then (el.x <- 0);
    (* if ((x#get_y + x#get_height) >= y_bounds) then (x#add_y (y_bounds - x#get_height))
         else*) if (el.y < 0) then(el.y <- 0)

  let correct_w_h el =                                                    (*the function checks if the elements are of correct height and width 1..10*)
    if (el.height<1) then (el.height <- 1)
    else if (el.height>10) then (el.height <- 10);
    if (el.width<1) then (el.width <- 1)
    else if (el.width>10) then (el.width <- 10)
end

type t = Element.t list

let sort_list t = List.sort Element.compare1 t                            (*the function is used to sort an array by y from min to max*)

let move_away (t, (el:Element.t)) =                                       (*the function move_away checks if element collides with others, and moves the element down if it does,*)
  let t = sort_list t in
  List.iter (fun x ->                                                     (*setting the is_moved value to true and y_bc (y before collision) to its previous y position *)
      while (Element.collides el x == true)&&(x.static == false)          (*&&(x#get_y <= y_bounds)*)
      do
        if (x!=el) then
          (
            (*if x#get_is_moved == false then (x#add_y_bc x#get_y);
            x#add_is_moved true;*)
            x.y <- (x.y + 1);
          )
      done
    ) t

let vertical_compacting (t,(el:Element.t)) =                               (*the function is used to move elements up while they dont reach the bounds or collide with others*)
  let t = sort_list t in
  let element_above = ref false in
  let el1 = Element.create 1 1 1 1 false (Dom_html.createDiv Dom_html.document) in
  el1.x <- el.x;
  el1.width <- el.width;
  el1.height <- el.height;
  el1.y <- (el.y-1);
  List.iter (fun x -> if (x!=el)&&(Element.collides x el1) then element_above := true)t;
  while (el.static == false)&&(not !element_above)&&(el1.y>=0)
  do
    List.iter (fun x ->
        if (Element.collides x el1)&&(x!=el) then
          (
            element_above := true;
          )
      ) t;
    if (not !element_above) then
      (
        el.y <- el1.y;
      );
    el1.y <- (el1.y-1);
  done
(*
let get_back t =                                                             (*the function is used to get elements back after moving if their previous place is vacant*)
  List.iter (fun x ->
      if x#get_is_moved==true then
        (
          let buf = x#get_y in
          x#add_y x#get_y_bc;
          List.iter (fun y ->
              if (collides x y) then (x#add_y buf);) t;
        );
      x#add_is_moved false;)t *)

let redraw_elements t =                                                      (*the function redraws elements, but not fully, only their pos. and size*)
  List.iter (fun (x:Element.t) ->
      Style.left x.element (hor*x.x);
      Style.width x.element (hor*x.width-10);
      Style.height x.element (vert*x.height-10);
      Style.top x.element (vert*x.y);
    ) t

let redraw_y_except (l:Element.t) t =                                        (*the function redraws all elements' pos. and size, except the given one (is used while moving and resizing)*)
  List.iter (fun x ->
      if (x!=l) then (Style.top x.element (vert*x.y));
    ) t

let max_y (t:Element.t list) =
  let rec f = (fun max t ->
      (
        match (t:Element.t list) with
        | [] -> max
        | hd :: tl -> f (if (hd.y+hd.height) > max then (hd.y+hd.height) else max) tl)) in
  (match (t:Element.t list) with
   | [] -> failwith "find max: list is empty"
   | hd :: tl -> f (hd.y+hd.height) tl)

let drag_element (el:Element.t) d t =                                                           (*the function for dragging an element el in html (div?) d*)
  let v, send = E.create () in
  let _ = E.map vertical_compacting v in
  let m, send1 = E.create () in
  let _ = E.map move_away m in
  el.element##.onmousedown := Dom_html.handler                                                  (*el is an element, we create handler for event 'on mouse down'*)
      (fun ev ->
         let shift_x = ev##.clientX - (el.x * hor) in                                           (*shift_x and shift_y are values that defines a shift between the cursor and element's real pos.*)
         let shift_y = ev##.clientY - (el.y * vert) in
         let ghost = Element.create_ghost el.x el.y el.width el.height "1" in                   (*ghost is a transparent element that shows user where and what size the element would be*)
         Dom.appendChild d ghost;
         let c1 =
           Dom_html.addEventListener Dom_html.document Dom_html.Event.mousemove                 (*here inside the handler we define event 'mouse move' and it's own hadler*)
             (Dom_html.handler
                (fun ev ->
                   let x_int = round(float_of_int(ev##.clientX-shift_x)/.float_of_int(hor)) in  (*x_int is used to define the pos. of element in time in 'columns'*)
                   Style.left el.element (ev##.clientX-shift_x);                                (*redraw the element's left (only visual)*)
                   el.x <- x_int;                                                               (*add the value to the object*)
                   let y_int = round(float_of_int(ev##.clientY-shift_y)/.float_of_int(vert)) in (*all the same but for Y*)
                   Style.top el.element (ev##.clientY-shift_y);
                   el.y <- y_int;
                   Element.correct_bounds el;                                                   (*correct the pos. of elements thar are outside the bounds*)
                   let t = sort_list t in
                   ignore (send1 (t,el));
                   if vc then List.iter (fun x -> send (t,x)) t;
                   List.iter (fun x -> send1 (t,x))t;
                   Style.height d (vert*(max_y t)-5);
                   Printf.printf "Div height is %d\n" (max_y t);
                   Style.top ghost (vert*el.y);                                                 (*redraw the ghost pos.*)
                   Style.left ghost (hor*el.x);
                   redraw_y_except el t;                                                        (*redraw all the elements except the moving one*)
                   Js._true))
             Js._true
         in
         let c2 = ref Js.null in
         c2 := Js.some
             (Dom_html.addEventListener Dom_html.document Dom_html.Event.mouseup                (*finally, when the mouse button is up, we:*)
                (Dom_html.handler
                   (fun _ ->
                      Element.correct_bounds el;
                      let t = sort_list t in                                                    (*correct the pos. of elements thar are outside the bounds*)
                      if vc then List.iter (fun x -> send (t,x))t;
                      (*else get_back !t;*)
                      List.iter (fun x -> send1 (t,x))t;
                      send1 (t,el);
                      redraw_elements t;                                                        (*redraw the elements*)
                      Element.draw_element el;
                      Style.height d (vert*(max_y t)-5);
                      Dom.removeChild d ghost;                                                  (*remove the ghost*)
                      Dom_html.removeEventListener c1;                                          (*remove event listener 'mouse move'*)
                      Js.Opt.iter !c2 Dom_html.removeEventListener;
                      Js._true))
                Js._true);
         Js._false)

let resize_element (el:Element.t) d t =                                                         (*the funtion for resizing an element*)
  let v, send = E.create () in
  let _ = E.map vertical_compacting v in
  let m, send1 = E.create () in
  let _ = E.map move_away m in
  let button,visual = Element.create_button () in                                               (*make the button and a visual part of it*)
  let mdx = ref 0 in
  let mdy = ref 0 in
  Dom.appendChild button visual;                                                                (*append the visual part to the button*)
  Dom.appendChild el.element button;
  button##.onmousedown := Dom_html.handler
      (fun ev ->
         Dom_html.stopPropagation ev;                            (*stop propagation stands here to evade the click on element and button at the same click and do it only for button instead*)
         let mx = ev##.clientX in                                                               (*mx is a value to remember where we started in px when the mouse is down*)
         let my = ev##.clientY in
         let ghost1 = Element.create_ghost el.x el.y el.width el.height "5" in                  (*here we create a ghost for resizing which is above all elements*)
         Dom.appendChild d ghost1;
         let c3 =
           Dom_html.addEventListener Dom_html.document Dom_html.Event.mousemove                 (*create an event mousemove and a handler for it*)
             (Dom_html.handler
                (fun ev ->
                   let x_px_delta = ev##.clientX-mx in                                          (*x px delta is used to define a change in x in pixels*)
                   Style.width el.element ((el.width*hor+x_px_delta)-10);                       (*redraw an element's width visually*)
                   Style.width el.obj ((el.width*hor+x_px_delta)-10);
                   let y_px_delta = ev##.clientY-my in                                          (*the same for y*)
                   Style.height el.element ((el.height*vert+y_px_delta)-10);                    (*the same for height*)
                   Style.height el.obj ((el.height*vert+y_px_delta)-10);
                   let x_delta_int = round(float_of_int(ev##.clientX-mx)/.float_of_int(hor)) in (*here we calculate the change in x 'columns'*)
                   let y_delta_int = round(float_of_int(ev##.clientY-my)/.float_of_int(vert)) in(*same for y*)
                   mdx := ev##.clientX;                                                         (*save the existing pos. of cursor*)
                   Style.width ghost1 ((el.width+x_delta_int)*hor-10);                          (*redraw the ghost's width*)
                   mdy := ev##.clientY;                                                         (*same for y*)
                   Style.height ghost1 ((el.height+y_delta_int)*vert-10);                       (*same for ghost's height*)
                   el.width <- (el.width + x_delta_int);                                        (*here we'r saving the new value of width*)
                   el.height <- (el.height + y_delta_int);                                      (*and height*)
                   Element.correct_w_h el;                                                      (*correct the pos. of elements thar are outside the bounds*)
                   let t = sort_list t in
                   ignore (send1 (t,el));
                   if vc then List.iter (fun x -> send (t,x)) t;
                   List.iter (fun x -> send1 (t,x)) t;
                   Style.height d (vert*(max_y t)-5);
                   el.width <- (el.width - x_delta_int);                                        (*we return the value to evade the continous incrementation*)
                   el.height <- (el.height - y_delta_int);                                      (*the same for y value*)
                   redraw_y_except el t;                                                        (*redraw all the element except the one that is being resized*)
                   Js._true))
             Js._true
         in
         let c4 = ref Js.null in
         c4 := Js.some
             (Dom_html.addEventListener Dom_html.document Dom_html.Event.mouseup                (*and finally when the mouse is up we:*)
                (Dom_html.handler
                   (fun _ ->
                      let x_delta_int = round(float_of_int(!mdx-mx)/.float_of_int(hor)) in      (*calculate the int pos in columns again*)
                      let y_delta_int = round(float_of_int(!mdy-my)/.float_of_int(vert)) in     (*the same for y*)
                      el.width <- (el.width + x_delta_int);                                     (*now we can update the values*)
                      el.height <- (el.height + y_delta_int);
                      Element.correct_w_h el;                                                   (*the function corrects width if width and height are <0 or >10*)
                      Element.correct_bounds el;                                                (*the function checks if the elements are within bounds*)
                      Style.width el.element (hor*el.width-10);                                 (*redraw element's width*)
                      Style.height el.element (vert*el.height-10);                              (*the same for height*)
                      Style.height el.obj (el.height*vert-10);
                      Style.width el.obj (el.width*hor-10);
                      let t = sort_list t in
                      if vc then List.iter (fun x -> send (t,x)) t;
                      (* else get_back t;*)
                      List.iter (fun x -> send1 (t,x)) t;
                      send1 (t,el);
                      redraw_elements t;                                                        (*redraw all the elements*)
                      Style.height d (vert*(max_y t)-5);
                      Dom.removeChild d ghost1;                                                 (*remove the ghost*)
                      Dom_html.removeEventListener c3;                                          (*stop listening mouse move*)
                      Js.Opt.iter !c4 Dom_html.removeEventListener;
                      Js._true))
                Js._true);
             Js._false)

let remove_element (el:Element.t) d t =                                                         (*the function removes an html element and truly DELETES the element from the list*)
  let v, send = E.create () in
  let _ = E.map vertical_compacting v in
  let m, send1 = E.create () in
  let _ = E.map move_away m in
  Dom.removeChild d el.element;
  let t1 = List.filter (fun (x:Element.t) -> x != el) t in
  if vc then List.iter (fun x -> send (t1,x)) t1;
 (* else get_back t;*)
  List.iter (fun x -> send1 (t1,x)) t1;
  Printf.printf "The num. of elements: %d\n" (List.length t1);
  Style.height d (vert*(max_y t1)-5);
  Printf.printf "Div height is %d\n" (max_y t1);
  List.iter (fun x -> drag_element x d t1; resize_element x d t1) t1;
  redraw_elements t1;
  t1

let add_element obj d t x y w h s=
  let m, send1 = E.create () in
  let _ = E.map move_away m in
  let v, send = E.create () in
  let _ = E.map vertical_compacting v in
  let el1 = Element.create x y w h s obj in
  Style.width el1.obj (el1.width*hor-10);
  Style.height el1.obj (el1.height*vert-10);
  Dom.appendChild el1.element el1.obj;
  Dom.appendChild d el1.element;
  Element.draw_element el1;
  let l1 = [el1] in
  let t1 = List.append t l1 in
  if vc then List.iter (fun x -> send (t1,x))t1;
  List.iter (fun x -> send1 (t1,x)) t1;
  Printf.printf "The num. of elements: %d\n" (List.length t1);
  Style.height d (vert*(max_y t1)-5);
  Printf.printf "Div height is %d\n" (max_y t1);
  List.iter (fun x -> drag_element x d t1; resize_element x d t1) t1;
  redraw_elements t1;
  t1

let serialize layout =
  List.map (fun (x:Element.t) -> ((x.x*hor),(x.y*vert),(x.width*hor-10),(x.height*vert-10),x.static))
    layout
end
