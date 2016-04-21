(** OCM configuration information (from .ocmrc and the command-line).

    @author eaburns
    @since 2010-08-24
*)

open Verb

let ocamlyacc_default = "ocamlyacc"
let ocamllex_default = "ocamllex"
let ocamldep_default = "ocamldep"
let ocamlc_default = "ocamlc"
let ocamlopt_default = "ocamlopt"
let ocamlmklib_default = "ocamlmklib"
let cc_default = "ocamlc"
let otags_default = "otags"
let build_dir_default = "_ocm"

let ocamlyacc = ref ocamlyacc_default
let ocamllex = ref ocamllex_default
let ocamldep = ref ocamldep_default
let ocamlc = ref ocamlc_default
let ocamlopt = ref ocamlopt_default
let ocamlmklib = ref ocamlmklib_default
let cc = ref cc_default
let otags = ref otags_default
let build_dir = ref build_dir_default

let roots = ref Fname.Set.empty


let to_string () =
  let b = Buffer.create 100 in
    Buffer.add_string b "ocamlyacc: ";
    Buffer.add_string b !ocamlyacc;
    Buffer.add_string b "\nocamllex: ";
    Buffer.add_string b !ocamllex;
    Buffer.add_string b "\nocamldep: ";
    Buffer.add_string b !ocamldep;
    Buffer.add_string b "\nocamlc: ";
    Buffer.add_string b !ocamlc;
    Buffer.add_string b "\nocamlopt: ";
    Buffer.add_string b !ocamlopt;
    Buffer.add_string b "\nocamlmklib: ";
    Buffer.add_string b !ocamlmklib;
    Buffer.add_string b "\ncc: ";
    Buffer.add_string b !cc;
    Buffer.add_string b "\notags: ";
    Buffer.add_string b !otags;
    Buffer.add_string b "\nDirs:\n";
    Fname.Set.iter (fun d ->
		      Buffer.add_char b '\t';
		      Buffer.add_string b d;
		      Buffer.add_char b '\n';)
      !roots;
    Buffer.contents b


(** [load parse_config path] loads the config file from the given
    path.  If there is no file at the path then just return the
    default configuration.  *)
let load parse_config path =
  if not (Sys.file_exists path) then
    vprintf ~lvl:verb_debug "Loaded config: %s (does not exist)\n" path
  else begin
    let inch = open_in path in
    let lb = Lexing.from_channel inch in
      parse_config lb;
      close_in inch;
      vprintf ~lvl:verb_debug "Loaded config: %s\n" path;
  end


let set_value vl = function
  | "ocamlyacc" -> ocamlyacc := vl
  | "ocamllex" -> ocamllex := vl
  | "ocamldep" -> ocamldep := vl
  | "ocamlc" -> ocamlc := vl
  | "ocamlopt" -> ocamlopt := vl
  | "ocamlmklib" -> ocamlmklib := vl
  | "cc" -> cc := vl
  | "otags" -> otags := vl
  | "build-dir" -> build_dir := vl
  | x -> invalid_arg x
