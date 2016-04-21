(** This file contains the core of the compilation engine.  The design
    is a simple graph of objects.  Each object has a list of source
    files, dependencies and an action te build it.

    Sets of objects are built at a time using the [ensure] function.
    Building sets of objects at a time allows for parallelism.

    An object is build by moving to its destination directory and
    either executing its command or function.  In order to get descent
    error messages from compile commands the commands should use a
    relative path for their source files.

    @author eaburns
    @since 2010-12-05
*)

open Printf
open Verb

type action =
  | Cmd of string
  | Fun of (unit -> unit)

type t = {
  obj : string;
  dir : string;
  srcs : string list;
  action : action;
  mutable deps : t list;
}

(** The maximum number of processes to spawn when building sets of
    objects. *)
let nprocs = ref 1

(** When true, will force objects to be built even if they are
    up-to-date. *)
let force = ref false

(** Set to true if there were any build failures.  Should check this
    before exiting to change the return status of the program on
    error. *)
let build_failures = ref false


(** {1 HashedType} *)

let hash x = Hashtbl.hash x

let equal a b = a.obj = b.obj

(** {1 Building objects} *)

(** Makes a new object node. *)
let make ~obj ~srcs ~action deps =
  {
    obj = obj;
    dir = Filename.dirname obj;
    srcs = srcs;
    action = action;
    deps = deps;
  }

(** Tests if the object file is up to date. *)
let up_to_date t =
  let rec check = function
    | f :: fs ->
	let s_obj = Unix.stat t.obj in
	let s_dep = Unix.stat f in
	  s_dep.Unix.st_mtime <= s_obj.Unix.st_mtime && check fs
    | [] -> true
  in
    check t.srcs && check (List.map (fun o -> o.obj) t.deps)

(** Test if the object needs to be rebuilt. *)
let needs_rebuild o =
  let dir = o.dir in
    if Sys.getcwd () <> dir then Sys.chdir dir;
    !force || not (Sys.file_exists o.obj) || not (up_to_date o)

(** Builds the given object. *)
let build o =
  let dir = o.dir in
    if Sys.getcwd () <> dir then Sys.chdir dir;
    match o.action with
      | Cmd cmd ->
	  vprintf ~lvl:verb_optional "%s\n" cmd;
	  if Verb_level.get () <= verb_optional then
	    vprintf ~lvl:verb_normal "Building %s\n" (Filename.basename o.obj);
	  let status = Sys.command cmd in
	    if status <> 0 then
	      failwith (sprintf "Command [%s] failed with status %d"
			  cmd status);
      | Fun f ->
	  vprintf ~lvl:verb_debug "Executing function for %s\n" o.obj;
	  f ()

(** Dependency graph among the objects. *)
module G = struct
  type node = t
  type graph = node list
  let iter objs f = List.iter f objs
  let succs _ obj = obj.deps
  let equal a b = a.obj = b.obj
  let hash = Hashtbl.hash
  let print_node ochan obj = fprintf ochan "%s" obj.obj
end

(** Builds the objects in serial. *)
let serial_build objs =
  let module G = Graph.Algs(G) in
  let objs = G.topo_sort objs in
  let rec build_objs = function
    | o :: os ->
	let n = build_objs os in
	  Fname.ensure_directory o.dir;
	  if needs_rebuild o then begin
	    begin try
	      build o
	    with Failure s ->
	      vprintf ~lvl:verb_critical "Build of %s failed: %s\n" o.obj s;
	      build_failures := true
	    end;
	    n + 1
	  end else
	    n
    | [] -> 0
  in
    build_objs objs

(** Distribute the objects [os] amongst the queues [qs].

    @param nbuilt Is a reference that counts the number of objects
    that need to be rebuilt. *)
let distribute_objects nbuilt qs os =
  let ps = !nprocs in
  let p = ref 0 in
    List.iter (fun o ->
		 Fname.ensure_directory o.dir;
		 if needs_rebuild o then begin
		   qs.(!p) <- o :: qs.(!p); p := (!p + 1) mod ps;
		   incr nbuilt
		 end)
      os

(** Spawn a child process. *)
let spawn_child q =
  if Unix.fork () = 0 then begin
    List.iter build q;
    exit 0
  end

(** Builds the objects by spawning multiple sub-processes. *)
let parallel_build objs =
  let nbuilt = ref 0 in
  let build_height os =
    if not !build_failures then begin
      let qs = Array.create !nprocs [] in
	distribute_objects nbuilt qs os;
	Array.iter spawn_child qs;
	for i = 1 to !nprocs do
	  match Unix.wait () with
	    | _, Unix.WEXITED 0 -> ()
	    | _, _ -> build_failures := true
	done
    end in
  let module G = Graph.Algs(G) in
  let heights = G.topo_heights objs in
    Array.iter build_height heights;
    !nbuilt

(** Ensures that a set of objects all exist. *)
let ensure objs =
  let cur_dir = Sys.getcwd () in
    (* preserve the directory across this function call since
       subroutines may change to intermediate directories. *)
  let nbuilt =
    if !nprocs > 1 then parallel_build objs else serial_build objs
  in
    Sys.chdir cur_dir;
    nbuilt

(** Gets a list of objects that is topologically sorted so that all of
    an object's dependencies are before it in the list. *)
let sorted objs =
  let module G = Graph.Algs(G) in
    List.rev (G.topo_sort objs)
