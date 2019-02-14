#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/fail.h>
#include <caml/custom.h>
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

value caml_cyusb_conn_num (value Unit) {
        int n;

        n = cyusb_open();

        cyusb_close();

        if (n < 0)
                caml_failwith ("Error in claiming interface");

        return Int_val (n);
}
        

value caml_cyusb_open (value n) {
        int            desc, max_in, max_out;
        unsigned short vendor, product;
        int            busnumber, devaddr;
        cyusb_handle*  tmp_handle = NULL;
        int            num;
        value          res, handl;

        num = Int_val (n);

        desc = cyusb_open();

        if ( desc < 0 || desc > num) { // TODO >=
                cyusb_close ();
                caml_failwith ("No such device num");
        };
                
        tmp_handle = cyusb_gethandle(num);

        if ( cyusb_getvendor(tmp_handle) != 0x04b4 ) {
                cyusb_close ();
                caml_failwith ("Cypress chipset not detected");
        }

        vendor = cyusb_getvendor(tmp_handle);
        product = cyusb_getproduct(tmp_handle);
        busnumber = cyusb_get_busnumber(tmp_handle);
        devaddr = cyusb_get_devaddr(tmp_handle);

        desc = cyusb_kernel_driver_active(tmp_handle, num);

        if ( desc != 0 ) {
                cyusb_close ();
                caml_failwith ("USB kernel driver is not active");
        }

        desc = cyusb_claim_interface(tmp_handle, num);

        if ( desc != 0 ) {
                cyusb_close ();
                caml_failwith ("Error in claiming interface");
        }

        max_in = cyusb_get_max_iso_packet_size(tmp_handle, IN_POINT);
        max_out = cyusb_get_max_iso_packet_size(tmp_handle, OUT_POINT);
        
        res = caml_alloc_tuple (7);

        handl = alloc_custom (&cyusb_handle_ops, sizeof(cyusb_handle*), 0, 1);
        Handler_val(handl) = tmp_handle;

        Store_field (res, 0, handl);
        Store_field (res, 1, Val_int (max_in));
        Store_field (res, 2, Val_int (max_out));
        Store_field (res, 3, Val_int (vendor));
        Store_field (res, 4, Val_int (product));
        Store_field (res, 5, Val_int (busnumber));
        Store_field (res, 5, Val_int (devaddr));

        return res;
}

value caml_cyusb_close (value Unit) {
        cyusb_close ();

        return Unit;
}

value caml_test_fun (value v) {
        int x = Int_val(v);
        char* buf = (char*) malloc (x);
        long dims[1] = { x };
        value ba = caml_ba_alloc (CAML_BA_CHAR | CAML_BA_C_LAYOUT | CAML_BA_MANAGED,
                                  1,
                                  buf,
                                  dims);
        caml_release_runtime_system();
        for (int i = 0; i < x; i++) {
                buf[i] = 0;
        };
        printf ("Test: bigarray of size %d is allocated\n", x);
        caml_acquire_runtime_system();
        return ba;
}
