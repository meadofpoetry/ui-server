open Js_of_ocaml

let is_enabled () : bool =
  let doc = Js.Unsafe.coerce Dom_html.document in
  let test x =
    if Js.Optdef.test x
    then Js.to_bool (Js.Optdef.get x (fun () -> assert false))
    else false in
  test doc##.fullscreenEnabled
  || test doc##.mozFullScreenEnabled
  || test doc##.msFullscreenEnabled
  || test doc##.webkitSupportsFullscreen
  || test doc##.webkitFullscreenEnabled
  || (let elt = Dom_html.(createVideo document) in
      (* Required for Opera Presto 12.14 *)
      Js.Optdef.test
      @@ (Js.Unsafe.coerce elt)##.webkitRequestFullScreen)

let fullscreen_element () : Dom_html.element Js.t option =
  let doc = Dom_html.document in
  if Js.Optdef.test (Js.Unsafe.coerce doc)##.fullscreenElement
  then
    let elt = (Js.Unsafe.coerce doc)##.fullscreenElement in
    Js.Opt.to_option elt
  else if Js.Optdef.test (Js.Unsafe.coerce doc)##.msFullscreenElement
  then
    let elt = (Js.Unsafe.coerce doc)##.msFullscreenElement in
    Js.Optdef.to_option elt
  else None

let is_fullscreen () : bool =
  let doc = Js.Unsafe.coerce Dom_html.document in
  let test_bool x =
    if Js.Optdef.test x
    then Js.to_bool (Js.Optdef.get x (fun () -> assert false))
    else false in
  let test_elt () = match fullscreen_element () with
    | None -> false
    | Some _ -> true in
  test_bool doc##.fullScreen
  || test_bool doc##.webkitIsFullScreen
  || test_bool doc##.mozFullScreen
  || test_elt doc##.msFullscreenElement

let (events : string list) =
  [ "fullscreenchange"
  ; "webkitfullscreenchange"
  ; "mozfullscreenchange"
  ; "msfullscreenchange"
  ]

let enter (elt : #Dom.node Js.t) : unit =
  let test = Js.Optdef.test in
  let elt = Js.Unsafe.coerce elt in
  if test elt##.requestFullscreen
  then elt##requestFullscreen
  else if test elt##.mozRequestFullScreen
  then elt##mozRequestFullScreen
  else if test elt##.webkitRequestFullScreen
  then elt##webkitRequestFullScreen
  else if test elt##.msRequestFullscreen
  then elt##msRequestFullscreen

let cancel () : unit =
  let test = Js.Optdef.test in
  let doc = Js.Unsafe.coerce Dom_html.document in
  if test doc##.exitFullscreen
  then doc##exitFullscreen
  else if test doc##.mozCancelFullScreen
  then doc##mozCancelFullScreen
  else if test doc##.webkitCancelFullScreen
  then doc##webkitCancelFullScreen
  else if test doc##.msExitFullscreen
  then doc##msExitFullscreen
