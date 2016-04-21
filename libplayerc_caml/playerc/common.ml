(** Common functions and declarations shared throughout playerc.

    @author eaburns
    @since 2010-01-30
*)

exception Playerc_error of string
  (* An error message returned from libplayerc. *)

type accessmode = Open | Close | Error
    (* If the order of these changes then it must be changed in
       libplayerc_caml/common.h *)

type datamode = Push | Pull
    (* If the order of these changes then it must be changed in
       libplayerc_caml/common.h *)

type transport = Tcp | Udp
    (* If the order of these changes then it must be changed in
       libplayerc_caml/common.h *)

let _ =
  Callback.register_exception "exception Playerc_error" (Playerc_error "");
  (* Register the Playerc_error for usage in the C stubs. *)

