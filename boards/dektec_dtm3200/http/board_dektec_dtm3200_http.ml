open Application_types
open Netlib.Uri
open Board_dektec_dtm3200_protocol

module Api_http = Api_cohttp.Make(User)(Body)

module Api_websocket = Api_websocket.Make(User)(Body)

(* TODO improve doc strings *)

let handlers (control : int) (api : Protocol.api) =
  let open Api_http in
  [ merge ~prefix:(Topology.get_api_path control)
      [ make ~prefix:"device"
          [ node ~doc:"Resets the board"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.("reset" @/ empty)
              ~query:Query.empty
              (Api_device.reboot api)
          ; node ~doc:"Returns the state of the board"
              ~meth:`GET
              ~path:Path.Format.("state" @/ empty)
              ~query:Query.empty
              (Api_device.get_state api)
          ; node ~doc:"Returns board description, if available"
              ~meth:`GET
              ~path:Path.Format.("info" @/ empty)
              ~query:Query.empty
              (Api_device.get_devinfo api)
          ; node ~doc:"Returns board configuration"
              ~meth:`GET
              ~path:Path.Format.("config" @/ empty)
              ~query:Query.empty
              (Api_device.get_config api)
          ]
      ; make ~prefix:"network"
          [ node ~doc:"Returns network config"
              ~meth:`GET
              ~path:Path.Format.("config" @/ empty)
              ~query:Query.empty
              (Api_network.get_config api)
          ; node ~doc:"Returns IP address of the device"
              ~meth:`GET
              ~path:Path.Format.("ip-address" @/ empty)
              ~query:Query.empty
              (Api_network.get_ip_address api)
          ; node ~doc:"Returns subnet mask"
              ~meth:`GET
              ~path:Path.Format.("subnet-mask" @/ empty)
              ~query:Query.empty
              (Api_network.get_subnet_mask api)
          ; node ~doc:"Returns gateway"
              ~meth:`GET
              ~path:Path.Format.("gateway" @/ empty)
              ~query:Query.empty
              (Api_network.get_gateway api)
          ; node ~doc:"Returns DHCP"
              ~meth:`GET
              ~path:Path.Format.("dhcp" @/ empty)
              ~query:Query.empty
              (Api_network.get_dhcp api)
          ; node ~doc:"Sets IP address of the device"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.("ip-address" @/ empty)
              ~query:Query.empty
              (Api_network.set_ip_address api)
          ; node ~doc:"Sets subnet mask"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.("subnet-mask" @/ empty)
              ~query:Query.empty
              (Api_network.set_subnet_mask api)
          ; node ~doc:"Sets gateway"
              ~restrict:[`Guest]
              ~meth:`POST
              ~path:Path.Format.("gateway" @/ empty)
              ~query:Query.empty
              (Api_network.set_gateway api)
          ; node ~doc:"Sets DHCP"
              ~meth:`POST
              ~path:Path.Format.("dhcp" @/ empty)
              ~query:Query.empty
              (Api_network.set_dhcp api)
          ]
      ]
  ]
