type error = [ `To_be_defined ]

type address = Ipaddr.t * mask
and mask = int32

type route =
  | Gateway of Ipaddr.t
  | Static of address list

type conn_method = DHCP | Manual

type eth_settings =
  { mac_address : Macaddr.t
  ; autoconnect : bool
  ; meth : conn_method
  ; address : address
  ; route : route
  ; dns : Ipaddr.t list
  }

type ethernet =
  < name : string

  ; get_settings : eth_settings Lwt.t

  ; apply_settings : 'a. eth_settings -> (unit, [> error ] as 'a) Lwt_result.t

  >

val ethernet_devices : unit -> ethernet list Lwt.t
