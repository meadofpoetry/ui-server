module Position = Dynamic_grid.Position

module Item : sig

  type settings =
    { widget : Widget.t
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
    ; widget      : Widget.t
    }

  type 'a positioned_item =
    { item     : 'a
    ; position : Position.t
    }

  val to_info : ?description:string -> ?thumbnail:[`Icon of string] ->
                serialized:Yojson.Safe.json -> title:string -> unit -> info
  val to_item : ?settings:settings -> name:string -> #Widget.t -> item

  class t :
          item:item -> unit ->
          object
            inherit Widget.t
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

type 'a typ = 'a * Item.t

class ['a] grid :
        'a #factory -> unit ->
        object
          inherit ['a typ,
                   'a typ Dynamic_grid.Item.t,
                   'a Item.positioned_item] Dynamic_grid_abstract.t

          method editable        : bool
          method set_editable    : bool -> unit
        end

class ['a] t :
        items:'a Item.positioned_item list -> 'a #factory -> unit ->
        object
          inherit Box.t

          method e_edited        : Yojson.Safe.json React.event

          method grid            : 'a grid
          method serialize       : unit -> Yojson.Safe.json
          method deserialize     : Yojson.Safe.json -> ('a Item.positioned_item list,string) result
          method restore         : Yojson.Safe.json -> (unit,string) result
        end
