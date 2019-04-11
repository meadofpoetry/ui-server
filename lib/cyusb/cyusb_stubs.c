#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/threads.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/memory.h>
//#include <lwt_unix.h>

#include <stdio.h>
#include <malloc.h>

#include <cyusb.h>

#define OUT_POINT 0x02
#define IN_POINT 0x86

#define MAX_BUF 8192

static struct custom_operations cyusb_handle_ops = {
        "cyusb_handler",
        custom_finalize_default,
        custom_compare_default,
        custom_hash_default,
        custom_serialize_default,
        custom_deserialize_default,
        custom_compare_ext_default
};

#define Handler_val(v) (*((cyusb_handle **) Data_custom_val(v)))

enum record_fields
{ Handler,
  Max_in,
  Max_out,
  Vendor,
  Product,
  Busnumber,
  Devaddr,
  Fields_num};

CAMLprim value caml_cyusb_open (value _unit) {
        CAMLparam0     ();
        CAMLlocal2     (res, handl);

        int            desc, max_in, max_out;
        unsigned short vendor, product;
        int            busnumber, devaddr;
        cyusb_handle*  tmp_handle = NULL;
        int            num;

        desc = cyusb_open();

        if ( desc != 1 ) {
                cyusb_close ();
                caml_failwith ("No device attached");
        };

        tmp_handle = cyusb_gethandle(0);

        if ( cyusb_getvendor(tmp_handle) != 0x04b4 ) {
                cyusb_close ();
                caml_failwith ("Cypress chipset not detected");
        }

        vendor = cyusb_getvendor(tmp_handle);
        product = cyusb_getproduct(tmp_handle);
        busnumber = cyusb_get_busnumber(tmp_handle);
        devaddr = cyusb_get_devaddr(tmp_handle);

        desc = cyusb_kernel_driver_active(tmp_handle, 0);

        if ( desc != 0 ) {
                cyusb_close ();
                caml_failwith ("USB kernel driver is not active");
        }

        desc = cyusb_claim_interface(tmp_handle, 0);

        if ( desc != 0 ) {
                cyusb_close ();
                caml_failwith ("Error in claiming interface");
        }

        max_in = cyusb_get_max_iso_packet_size(tmp_handle, IN_POINT);
        max_out = cyusb_get_max_iso_packet_size(tmp_handle, OUT_POINT);

        res = caml_alloc_tuple (Fields_num);

        handl = alloc_custom (&cyusb_handle_ops, sizeof(cyusb_handle*), 0, 1);
        Handler_val(handl) = tmp_handle;

        Store_field (res, Handler, handl);
        Store_field (res, Max_in, Val_int (max_in));
        Store_field (res, Max_out, Val_int (max_out));
        Store_field (res, Vendor, Val_int (vendor));
        Store_field (res, Product, Val_int (product));
        Store_field (res, Busnumber, Val_int (busnumber));
        Store_field (res, Devaddr, Val_int (devaddr));

        CAMLreturn(res);
}

CAMLprim value caml_cyusb_close (value _h) {
         /* since H is unused we could ommit the root */
        CAMLparam0 ();

        cyusb_close ();

        CAMLreturn(Val_unit);
}

CAMLprim value caml_cyusb_send (value h, value ba) {
        CAMLparam2 (h, ba);

        cyusb_handle* handle = Handler_val(Field (h, Handler));
        int max_out = Int_val (Field (h, Max_out));
        size_t size = Caml_ba_array_val(ba)->dim[0];
        char* buf   = Caml_ba_data_val(ba);
        size_t len  = 0;
        int transfd = 0;
        int fail    = 0;

        caml_release_runtime_system();

        while (size > 0) {
                len = size < max_out ? size : max_out;

                if(cyusb_bulk_transfer(handle,
                                       OUT_POINT,
                                       buf,
                                       len,
                                       &transfd,
                                       500)) {
                        fail = 1;
                        goto cleanup;
                }

                size -= transfd;
                buf  += transfd;
        }

cleanup:
        caml_acquire_runtime_system();

        if (fail) {
                caml_failwith ("Cyusb: error on send");
        }
        CAMLreturn(Val_unit);
}

CAMLprim value caml_cyusb_recv (value h) {
        CAMLparam1 (h);
        CAMLlocal1 (ba);

        cyusb_handle* handle = Handler_val(Field (h, Handler));
        int max_in  = Int_val (Field (h, Max_in));
        char * buf  = (char*) malloc (MAX_BUF);
        long dims[1];
        int transfd = 0;
        size_t size = 0;
        size_t len  = 0;
        int fail    = 0;
        
        caml_release_runtime_system();

        do {
                len = max_in > MAX_BUF - size ? MAX_BUF - size : max_in;

                if(cyusb_bulk_transfer(handle,
                                       IN_POINT,
                                       &buf[size],
                                       len,
                                       &transfd,
                                       500)) {
                        fail = 1;
                        goto cleanup;
                }

                size += transfd;
                
        } while (transfd && size < MAX_BUF);

cleanup:
        caml_acquire_runtime_system();

        if (fail) {
                free(buf);
                caml_failwith ("Cyusb: error on recv");
        }

        buf = realloc(buf, size);

        dims[0] = size;
        ba = caml_ba_alloc (CAML_BA_CHAR | CAML_BA_C_LAYOUT | CAML_BA_MANAGED,
                            1,
                            buf,
                            dims);

        CAMLreturn (ba);
}
