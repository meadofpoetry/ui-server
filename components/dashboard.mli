module Position = Dynamic_grid.Position

module Item : sig

  type settings =
    { widget : Widget.widget
    ; ready  : bool React.signal
    ; set    : unit -> (unit,string) Lwt_result.t
    }

  type info =
    { title       : string
    ; description : string
    ; thumbnail   : [`Icon of string]
    ; serialized  : Yojson.Safe.json
    }

  type item =
    { name        : string
    ; settings    : settings option
    ; widget      : Widget.widget
    }

  type 'a positioned_item =
    { item     : 'a
    ; position : Position.t
    }

  val to_info : ?description:string -> ?thumbnail:[`Icon of string] ->
                serialized:Yojson.Safe.json -> title:string -> unit -> info
  val to_item : ?settings:settings -> name:string -> #Widget.widget -> item

  class t :
          item:item -> unit ->
          object
            inherit Widget.widget
            method remove       : Icon.Button.Font.t
            method content      : Card.Media.t
            method heading      : Card.Primary.t

            method editable     : bool
            method set_editable : bool -> unit
          end

end

type available = [ `List   of Item.info list
                 | `Groups of (string * Item.info list) list
                 ]

class type ['a] factory =
  object
    method create      : 'a -> Item.item
    method destroy     : unit -> unit
    method available   : available
    method serialize   : 'a -> Yojson.Safe.json
    method deserialize : Yojson.Safe.json -> ('a,string) result
  end

class ['a] grid :
        'a #factory -> unit ->
        object
          inherit Widget.widget

          method add             : 'a Item.positioned_item ->
                                   ('a Dynamic_grid.Item.t,Dynamic_grid_abstract.add_error) result
          method grid            : Dynamic_grid_abstract.grid
          method item_margin     : int * int
          method set_item_margin : int * int -> unit
          method items           : 'a Dynamic_grid.Item.t list
          method overlay_grid    : Dynamic_grid_abstract.overlay_grid
          method positions       : Position.t list
          method remove          : 'a Dynamic_grid.Item.t -> unit
          method remove_all      : unit -> unit
          method draggable       : bool option
          method set_draggable   : bool option -> unit
          method resizable       : bool option
          method set_resizable   : bool option -> unit
          method selectable      : bool option
          method set_selectable  : bool option -> unit

          method editable        : bool
          method set_editable    : bool -> unit

          method e_selected      : 'a Dynamic_grid.Item.t list React.event
          method s_change        : Position.t list React.signal
          method s_changing      : Position.t list React.signal
          method s_items         : 'a Dynamic_grid.Item.t list React.signal
          method s_selected      :'a Dynamic_grid.Item.t list React.signal
        end

class ['a] t :
        items:'a Item.positioned_item list -> 'a #factory -> unit ->
        object
          inherit Box.t

          method grid            : 'a grid
          method serialize       : unit -> Yojson.Safe.json
          method deserialize     : Yojson.Safe.json -> ('a Item.positioned_item list,string) result
          method restore         : Yojson.Safe.json -> (unit,string) result
        end
