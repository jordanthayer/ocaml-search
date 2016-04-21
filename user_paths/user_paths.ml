(** Info on the user's paths to things.
*)

let instance_root = "./group/data/" (* must end in '/' *)
  (** The root directory for instances. *)

let user_root_tab = [
  (** This is an associated list of (username * data_root) tuples.  It
      uses $USER to find the current user name and lookup the user's
      data root (where datafiles will be stored when the user performs
      runs).

      If you want to have data stored in an RDB as a result of running
      experiments, then this table is for you.  You need to add a
      tuple with your username and the directory into which you want
      your RDB.  Make sure this directory exists and *PLEASE* make
      sure the path in this table has a trailing '/', otherwise things
      will break. *)
]

let get_user_root () =
  (** Look up the user's root in the table... expands ~ into the
      user's home directory. *)
  let user =
    try Sys.getenv "USER"
    with Not_found -> failwith "$USER is not in the environment" in
  let raw_root =
    try (List.assoc user user_root_tab)
    with Not_found ->
      failwith (Printf.sprintf
		  "User %s has no data_root, please refer to experiments/experiments.ml for instructions on how to add a data_root for a user"
		  user)
  in Wrfname.expand_home raw_root

let get_user () =
  (** Look up the user's root in the table... expands ~ into the
      user's home directory. *)
  let user =
    try Sys.getenv "USER"
    with Not_found -> failwith "$USER is not in the environment" in
    user

let get_user_safe () =
  (** Identical to get user, but does not fail when no user is found, instead
      it returns nobody *)
  let user =
    try Sys.getenv "USER"
    with Not_found -> "nobody" in
    user


let data_root =
  (** The data root for the current user is the directory in which the
      user wants the data output as the results of runs to be
      placed. *)
  (get_user_root ()) ^ "data/"


let bin_root =
  (** The binary root for the current user is the directory in which the
      user wants the data output as the results of runs to be
      placed. *)
  (get_user_root ()) ^ "experiments/bins/"


let plot_root =
  (** The binary root for the current user is the directory in which the
      user wants the data output as the results of runs to be
      placed. *)
  (get_user_root ()) ^ "plots/"
