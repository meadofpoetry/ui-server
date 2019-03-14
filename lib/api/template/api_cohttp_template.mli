type resp = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

type script = Src of string
            | Raw of string

type tmpl_props =
  { title : string option
  ; pre_scripts : script list
  ; post_scripts : script list
  ; stylesheets : string list
  ; content : string list
  }

type priority = [ `Index of int | `None ]

type upper
   
type inner

type _ item =
  | Home    : tmpl_props
              -> upper item
  
  | Pure    : { path : (unit -> resp, resp) Netlib.Uri.Path.Format.t
              ; template : tmpl_props
              } -> upper item
  
  | Ref     : { title : string
              ; href  : Netlib.Uri.t
              } -> _ item
  
  | Simple  : { title : string
              ; icon  : Tyxml.Xml.elt option
              ; href  : Netlib.Uri.Path.t
              ; template : tmpl_props
              } -> _ item
  
  | Subtree : { title : string
              ; icon  : Tyxml.Xml.elt option
              ; href  : Netlib.Uri.Path.t
              ; templates : inner ordered_item list
              } -> upper item
  
and 'a ordered_item = (priority * 'a item)

val build_route_table : ?href_base:string
                        -> template:string
                        -> user:string
                        -> upper ordered_item list
                        -> resp Netlib.Uri.Dispatcher.t

module Priority : sig
  type t = priority
  val compare : t -> t -> int
end
