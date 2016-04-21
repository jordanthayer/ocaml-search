(** Add special ocamlbuild rules to link with libplayerc.

    @author eaburns
    @since 2010-01-28
*)

open Ocamlbuild_plugin
open Command

let playerc_libdir = "-L/usr/local/lib64"
let playerc_include = "-I/usr/local/include/player-3.1"

let static = true

let headers =
  (* Get all ".h" files in the libplayerc_caml directory. *)
  List.map
    (fun s -> "libplayerc_caml/" ^ s)
    (List.filter (fun s ->
		    let n = String.length s in
		      s.[n - 1] = 'h' && s.[n - 2] = '.')
       (Array.to_list (Sys.readdir "libplayerc_caml")))
;;

dispatch begin function
  | After_rules ->

      (************************************************************)
      (* Description of how to link with libplayerc.              *)
      (************************************************************)

      (* When making a C library that uses playerc with ocamlmklib,
         then issue these flags. *)
      flag ["ocamlmklib"; "c"; "use_playerc"]
        (S[A playerc_libdir; A "-lplayerc"]);

      (* When compiling C code including the playerc headers *)
      flag ["c"; "compile"; "include_playerc"]
        (S[A"-ccopt"; A playerc_include;]);

      (* Linking code that use playerc, then add a link flag. *)
      flag ["link"; "ocaml"; "use_playerc"]
        (S[A"-ccopt"; A playerc_libdir; A"-cclib"; A "-lplayerc"]);

      (************************************************************)
      (* Description of how to link with libplayerc_caml.         *)
      (************************************************************)

      ocaml_lib "libplayerc_caml";

      flag ["link"; "ocaml"; "byte"; "use_playerc_caml"]
        (S[A "-ccopt"; A playerc_libdir;
	   A"-cclib"; A"-lplayerc";
	   A"-cclib"; A"-lplayerc_caml";
	  ]);

      flag ["link"; "ocaml"; "native"; "use_playerc_caml"]
        (S[A "-ccopt"; A playerc_libdir;
	   A"-cclib"; A"-lplayerc";
	   A "-cclib"; A "-lplayerc_caml";
	  ]);

      (************************************************************)
      (* Dependencies for building c files are all the headers.   *)
      (************************************************************)

      (* Move the headers into the build directory when building
	 libplayerc_caml.a. *)
      dep ["compile"; "c"] headers;


      (************************************************************)
      (* If static is true then build everything with -custom.    *)
      (************************************************************)

      (* If `static' is true then every ocaml link in bytecode will
	 add -custom *)
      if static then flag ["link"; "ocaml"; "byte"] (A"-custom");


  | _ -> ()
end
