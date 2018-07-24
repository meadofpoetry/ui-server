open Containers
open Board_types
open Board_api_common
open Api.Interaction
open Api.Interaction.Json
open Common
open Types
   
(* let ts_handler db (api:api) events =
 *   let open Uri in
 *   let open Boards.Board.Api_handler in
 *   create_dispatcher
 *     "ts"
 *     [ create_ws_handler ~docstring:"Pushes available TS to the client"
 *         ~path:Path.Format.empty
 *         ~query:Query.[ "id", (module List(Int32)) ]
 *         (WS.TS.streams events)
 *     ; create_ws_handler ~docstring:"Pushes TS state change to the client"
 *         ~path:Path.Format.("state" @/ empty)
 *         ~query:Query.[ "id", (module List(Int32)) ]
 *         (WS.TS.state events)
 *     ; create_ws_handler ~docstring:"Pushes TS bitrates to the client"
 *         ~path:Path.Format.("bitrate" @/ empty)
 *         ~query:Query.[ "id", (module List(Int32)) ]
 *         (WS.TS.bitrate events)
 *     ; create_ws_handler ~docstring:"Pushes TS structure to the client"
 *         ~path:Path.Format.("structure" @/ empty)
 *         ~query:Query.[ "id", (module List(Int32)) ]
 *         (WS.TS.structure events)
 *     ]
 *     [ `GET,
 *       [ create_handler ~docstring:"Returns current TS list"
 *           ~path:Path.Format.empty
 *           ~query:Query.[ "id", (module List(Int32)) ]
 *           (HTTP.TS.streams events)
 *       ; create_handler ~docstring:"Returns current TS bitrate"
 *           ~path:Path.Format.("bitrate" @/ empty)
 *           ~query:Query.[ "id", (module List(Int32)) ]
 *           (HTTP.TS.bitrate api)
 *       ; create_handler ~docstring:"Returns current TS structure"
 *           ~path:Path.Format.("structure" @/ empty)
 *           ~query:Query.[ "id", (module List(Int32)) ]
 *           (HTTP.TS.structure api)
 *       (\* Archive *\)
 *       ; create_handler ~docstring:"Returns archived streams"
 *           ~path:Path.Format.("archive" @/ empty)
 *           ~query:Query.[ "id",       (module List(Int32))
 *                        ; "input",    (module List(Topology.Show_topo_input))
 *                        ; "limit",    (module Option(Int))
 *                        ; "from",     (module Option(Time.Show))
 *                        ; "to",       (module Option(Time.Show))
 *                        ; "duration", (module Option(Time.Relative)) ]
 *           (HTTP.TS.Archive.streams db)
 *       (\*  ; create_handler ~docstring:"Retunrs archived stream state"
 *                 ~path:Path.Format.("state/archive" @/ empty)
 *                 ~query:Query.[ "id",       (module List(Int32))
 *                              ; "limit",    (module Option(Int))
 *                              ; "compress", (module Option(Bool))
 *                              ; "from",     (module Option(Time.Show))
 *                              ; "to",       (module Option(Time.Show))
 *                              ; "duration", (module Option(Time.Relative)) ]
 *                 HTTP.TS.Archive.state;*\)
 *       ; create_handler ~docstring:"Retunrs archived stream bitrate"
 *           ~path:Path.Format.("bitrate/archive" @/ empty)
 *           ~query:Query.[ "id",       (module List(Int32))
 *                        ; "limit",    (module Option(Int))
 *                        ; "compress", (module Option(Bool))
 *                        ; "from",     (module Option(Time.Show))
 *                        ; "to",       (module Option(Time.Show))
 *                        ; "duration", (module Option(Time.Relative)) ]
 *           HTTP.TS.Archive.bitrate
 *       ; create_handler ~docstring:"Retunrs archived stream structure"
 *           ~path:Path.Format.("structure/archive" @/ empty)
 *           ~query:Query.[ "id",       (module List(Int32))
 *                        ; "limit",    (module Option(Int))
 *                        ; "from",     (module Option(Time.Show))
 *                        ; "to",       (module Option(Time.Show))
 *                        ; "duration", (module Option(Time.Relative)) ]
 *           (HTTP.TS.Archive.structure db)
 *       ]
 *     ]
 * 
 * let t2mi_handler db (api:api) events =
 *   let open Uri in
 *   let open Boards.Board.Api_handler in
 *   create_dispatcher
 *     "t2mi"
 *     [ create_ws_handler ~docstring:"Pushes stream state to the client"
 *         ~path:Path.Format.("state" @/ empty)
 *         ~query:Query.["id", (module List(Int))]
 *         (WS.T2MI.state events)
 *     ; create_ws_handler ~docstring:"Pushes stream structure to the client"
 *         ~path:Path.Format.("structure" @/ empty)
 *         ~query:Query.["id", (module List(Int))]
 *         (WS.T2MI.structure events)
 *     ]
 *     [ `GET, [ create_handler ~docstring:"Returns T2-MI stream structure (L1 signalling)"
 *                 ~path:Path.Format.("structure" @/ empty)
 *                 ~query:Query.[ "id", (module List(Int)) ]
 *                 (HTTP.T2MI.structure api)
 *             ; create_handler ~docstring:"Returns T2-MI packet sequence"
 *                 ~path:Path.Format.("sequence" @/ empty)
 *                 ~query:Query.[ "id",       (module List(Int))
 *                              ; "duration", (module Option(Time.Relative)) ]
 *                 (HTTP.T2MI.sequence api)
 *             (\* Archive *\)
 *            (\* ; create_handler ~docstring:"Returns archived stream state"
 *                 ~path:Path.Format.("state/archive" @/ empty)
 *                 ~query:Query.[ "id",       (module List(Int))
 *                              ; "limit",    (module Option(Int))
 *                              ; "compress", (module Option(Bool))
 *                              ; "from",     (module Option(Time.Show))
 *                              ; "to",       (module Option(Time.Show))
 *                              ; "duration", (module Option(Time.Relative)) ]
 *                 HTTP.T2MI.Archive.state *\)
 *             ; create_handler ~docstring:"Returns archived stream structure"
 *                 ~path:Path.Format.("structure/archive" @/ empty)
 *                 ~query:Query.[ "id",       (module List(Int))
 *                              ; "limit",    (module Option(Int))
 *                              ; "from",     (module Option(Time.Show))
 *                              ; "to",       (module Option(Time.Show))
 *                              ; "duration", (module Option(Time.Relative)) ]
 *                 (HTTP.T2MI.Archive.structure db)
 *             ]
 *     ]
 * 
 * let handlers db api events =
 *   [ ts_handler db api events
 *   ; t2mi_handler db api events
 *   ] *)
