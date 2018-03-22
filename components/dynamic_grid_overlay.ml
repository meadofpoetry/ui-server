open Containers

class overlay_grid ~parent () =
  let elt = Markup.Dynamic_grid.Overlay_grid.create () |> Tyxml_js.To_dom.of_element in
  object(self)

    inherit Widget.widget elt ()

    val mutable grid_color     = CSS.Color.RGBA (0,0,0,0.25)
    val mutable divider_color  = CSS.Color.RGBA (0,0,0,0.5)
    val mutable show_dividers  = false
    val mutable divider_period = 10,10

    method show = Dom.appendChild parent self#root;
                  parent##.classList##add (Js.string Markup.Dynamic_grid.with_overlay_grid_class)
    method hide = (try Dom.removeChild parent self#root with _ -> ());
                  parent##.classList##remove (Js.string Markup.Dynamic_grid.with_overlay_grid_class)

    method set_grid_color x     = grid_color <- x; self#layout
    method get_grid_color       = grid_color

    method set_divider_color x  = divider_color <- x; self#layout
    method get_divider_color    = divider_color

    method set_divider_period x = divider_period <- x; self#layout
    method get_divider_period   = divider_period

    method show_dividers = show_dividers <- true
    method hide_dividers = show_dividers <- false

    method layout cw rh im =
      let gc     = CSS.Color.string_of_t self#get_grid_color in
      let dc     = CSS.Color.string_of_t self#get_divider_color in
      let dividers =
        if not show_dividers then []
        else [ self#line_background_image (-90) dc (cw * (fst self#get_divider_period)) (fst im)
             ; self#line_background_image 0     dc (rh * (snd self#get_divider_period)) (snd im)
             ]
      in
      let lines = [ self#line_background_image (-90) gc cw (fst im)
                  ; self#line_background_image 0     gc rh (snd im)
                  ]
      in
      let bg_image   = String.concat "," (lines @ dividers) in
      let sz_w, sz_h = if show_dividers then cw * fst self#get_divider_period,
                                             rh * snd self#get_divider_period
                       else cw, rh in
      let bg_size    = Printf.sprintf "%dpx %dpx" sz_w sz_h in
      self#style##.backgroundImage := Js.string bg_image;
      ((Js.Unsafe.coerce self#style)##.backgroundSize := Js.string bg_size)
      |> ignore

    method private line_background_image degrees color size border =
      if size <= border
      then Printf.sprintf "linear-gradient(%s, %s)" color color
      else
        let line_start = size - border in
        let line_end   = size in
        Printf.sprintf "repeating-linear-gradient(%ddeg,transparent,transparent %dpx,%s %dpx,%s %dpx)"
                       degrees line_start color line_start color line_end

  end
