
module type USER = sig
  include Api.USER
  val to_string : t -> string
end

module Make (User : USER) : Template_intf.S with type user = User.t

(*
type resp = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

type script = Src of string
            | Raw of string

type template_props =
  { title : string option
  ; pre_scripts : script list
  ; post_scripts : script list
  ; stylesheets : string list
  ; content : string list
  }

type priority = [ `Index of int | `None ]
  
type topmost
   
type inner

type _ item
   
type 'a ordered_item = priority * 'a item

(* TODO
val home : template_props -> topmost item
 *)
val reference : title:string
                -> href:Netlib.Uri.t
                -> 'a item

val simple : title:string
             -> icon:Tyxml.Xml.elt option
             -> path:Netlib.Uri.Path.t
             -> template_props
             -> 'a item

val pure : path:('a, 'b) Netlib.Uri.Path.Format.t
           -> template_props
           -> 'c item
  
val subtree : title:string
             -> icon:Tyxml.Xml.elt option
             -> path:Netlib.Uri.Path.t
             -> inner ordered_item list
             -> topmost item

val make : template:string
           -> user:string
           -> topmost ordered_item list
           -> resp Netlib.Uri.Dispatcher.t

 *)

    (*
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

type topmost
   
type inner

type _ item =
  | Home    : tmpl_props
              -> topmost item
  
  | Pure    : { path : (unit -> resp, resp) Netlib.Uri.Path.Format.t
              ; template : tmpl_props
              } -> topmost item
  
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
              } -> topmost item
  
and 'a ordered_item = (priority * 'a item)

val build_route_table : ?href_base:string
                        -> template:string
                        -> user:string
                        -> topmost ordered_item list
                        -> resp Netlib.Uri.Dispatcher.t

module Priority : sig
  type t = priority
  val compare : t -> t -> int
end
           *)
