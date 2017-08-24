open Ctypes
open Foreign

type handl = unit ptr
let handl : handl typ = ptr void

let init = foreign "cyusb_open" (void @-> returning int)

let close = foreign "cyusb_close" (void @-> returning void)

let get_handle = foreign "cyusb_gethandle" (int @-> returning handl)

let get_vendor = foreign "cyusb_getvendor" (handl @-> returning ushort)

let get_product = foreign "cyusb_getproduct" (handl @-> returning ushort)

let get_bus_number = foreign "cyusb_get_busnumber" (handl @-> returning int)

let get_devaddr = foreign "cyusb_get_devaddr" (handl @-> returning int)

let kernel_driver_active = foreign "cyusb_kernel_driver_active" (handl @-> int @-> returning int)

let detach_kernel_driver = foreign "cyusb_detach_kernel_driver" (handl @-> int @-> returning int)

let attach_kernel_driver = foreign "cyusb_attach_kernel_driver" (handl @-> int @-> returning int)

let claim_interface = foreign "cyusb_claim_interface" (handl @-> int @-> returning int)

let get_max_iso_packet_size = foreign "cyusb_get_max_iso_packet_size" (handl @-> uchar @-> returning int)

let bulk_transfer = foreign "cyusb_bulk_transfer"
                            (handl @-> uchar @-> ptr char @-> int @-> ptr int32_t @-> int @-> returning int)

                  (*
                              Prototype    : void cyusb_bulk_transfer(cyusb_handle *h, unsigned char endpoint,
                     unsigned char *data, int length, int *transferred, int timeout);
  Description  : Performs a USB Bulk Transfer.
  Parameters   :
                 cyusb_handle *h        : Device handle
                 unsigned char endpoint : Address of endpoint to comunicate with
                 unsigned char *data    : Data Buffer ( for input or output )
                 unsigned short wLength : The length field of the data buffer for read or write
                 int * transferred      : Output location of bytes actually transferred
                 unsigned int timeout   : Timeout in milliseconds. 0 means no Timeout.
  Return Value : 0 on success, or an appropriate LIBUSB_ERROR.
                   *)
