(* I/O with pretty printing formatters *)


let with_pp ch f =
  (** calls [f] on a pretty-printing channel made from [ch] *)
  flush ch;
  let ch = Format.formatter_of_out_channel ch in
    Wrutils.unwind_protect f ch
      (fun () -> Format.pp_print_flush ch ()) ()


(* EOF *)
