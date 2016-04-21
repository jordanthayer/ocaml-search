(** Types of things that may appear in a system file.

    @author eaburns
    @since 2011-02-10
*)

type file = {
  file_path : string;
  file_cmp_flags : string list;
  file_lnk_flags : string list;
  file_pp : string list;
}


type result_kind =
  | Use
  | Tags
  | Byte_debug
  | Native
  | Native_debug
  | Native_prof
  | Native_unsafe

type result = {
  res_name : string;
  res_kind : result_kind;
  res_files : file list;
}

type system = {
  sys_name : string;
  sys_path : string;
  sys_dotfiles : string list;
  sys_systems : string list;
  sys_files : file list;
  sys_extensions : file list;
  sys_libs : (string * string) list;
  sys_clibs : (string * string) list;
  sys_results : result list;
}

let dummy_file =
  {
    file_path = "";
    file_cmp_flags = [];
    file_lnk_flags = [];
    file_pp = [];
  }


let dummy_res = { res_name = "<dummy>"; res_files = []; res_kind = Native }


let dummy_sys =
  { sys_name = "";
    sys_path = "";
    sys_dotfiles = [];
    sys_systems = [];
    sys_files = [];
    sys_extensions = [];
    sys_libs = [];
    sys_clibs = [];
    sys_results = []; }
