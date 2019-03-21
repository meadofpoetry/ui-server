module type S = sig

  type user

  type script = Src of string
              | Raw of string

  type template_props =
    { title : string option
    ; pre_scripts : script list
    ; post_scripts : script list
    ; stylesheets : string list
    ; content : string list
    }

  type topmost
   
  type inner

  type priority = [ `Index of int | `None ]

  type 'a item

  val reference : ?restrict:user list
                  -> ?priority:priority
                  -> title:string
                  -> href:Netlib.Uri.t
                  -> 'a item list

  val simple : ?restrict:user list
               -> ?priority:priority
               -> title:string
               -> ?icon:Tyxml.Xml.elt
               -> path:Netlib.Uri.Path.t
               -> template_props
               -> 'a item list

  val parametric : ?restrict:user list
                   -> path:('a, user -> Mustache.Json.value option) Netlib.Uri.Path.Format.t
                   -> template_props
                   -> 'c item list
    
  val subtree : ?restrict:user list
                -> ?priority:priority
                -> title:string
                -> ?icon:Tyxml.Xml.elt
                -> inner item list
                -> topmost item list

  val make : template:string
             -> topmost item list
             -> (Cohttp.Code.meth
                 * (user -> 'a -> 'b -> 'c ->
                    [> `Instant of  (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
                    |  `Error of string ] Lwt.t) Netlib.Uri.Dispatcher.node) list

end
