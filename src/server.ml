open Lwt
open Lwt_react
open Cohttp_lwt_unix
open Containers
open Api

let (%) = Fun.(%)

let home () =
  Cohttp_lwt_unix.Server.respond_string
               ~status:`OK
               ~body: {| 
                       <html>
                       <head>
                       <meta charset="utf-8">
                       <script src="//code.jquery.com/jquery-1.11.3.min.js"></script>
                       <script>
                       $(window).on('load', function(){
                       ws = new WebSocket('ws://localhost:7777/ws');
                       ws.onmessage = function(x) {
                       $('#msg').html("<p>" + x.data + "</p>");
                       };
                       });
                       </script>
                       </head>
                       <body>
                       <div id='msg'></div>
                       </body>
                       </html>
                       |}
               ()

let login () =
  Cohttp_lwt_unix.Server.respond_file ~fname:"login" ()
        
let handler
      (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
      (req  : Cohttp_lwt_unix.Request.t)
      (body : Cohttp_lwt_body.t) =
  Uri.path @@ Cohttp.Request.uri req
  |> String.split_on_char '/'
  |> List.filter (not % String.equal "")
  |> function
    | []            -> home ()
    | ["login"]     -> login ()
    | "api" :: path -> Api.handle path
    | _             -> Cohttp_lwt_unix.Server.respond_not_found ()
             
   
let create () =
  Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 7777))
                                (Cohttp_lwt_unix.Server.make ~callback:handler ())
