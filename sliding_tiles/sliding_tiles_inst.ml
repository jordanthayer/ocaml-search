(** Sliding tiles instances.  After being read in, everything is
    stored in the 'contents' format and not the 'positions' format (to
    avoid *any* confusion).

    @author eaburns
    @since 2010-09-02
*)

open Printf

type inst = {
  nrows : int;
  ncols : int;

  glued : int list;

  nelms : int;
  (* Number of elements. *)

  initial : int array;
  (* Initial tile configuration. *)

  goal : int array;
  (* Goal tile configuration. *)
}

let blank_tile = 0

let reverse_inst i =
  { nrows = i.nrows;
    ncols = i.ncols;
    nelms = i.nelms;
    glued = i.glued;
    initial = Array.copy i.goal;
    goal = Array.copy i.initial;
  }

let contents_of_positions p =
  (** [contents_of_positions p] gets a contents array from a positions
      array. *)
  let len = Array.length p in
  let c = Array.create len ~-1 in
    Array.iteri (fun t p -> c.(p) <- t) p;
    c


let positions_of_contents c =
  (** [positions_of_contents c] gets a positions array from a contents
      array. *)
  let len = Array.length c in
  let p = Array.make len ~-1 in
    Array.iteri (fun i a -> p.(a) <- i) c;
    p



(** [is_solvable positions] tests if an instance is solvable given
    its initial configuration.  If it has an even number of
    reversals then it will be solvable. *)
let is_solvable pos =
  (* currently, this will say that things which are solvable are not
     and possibly vice versa, it's hard to prove a negative in a space too big
     to permit enumeration. *)
  let size = Array.length pos - 1 in
  let contents = contents_of_positions pos in
  let revord = ref 0
  and blank = ref 0 in
    for i = 0 to size do
      (if contents.(i) = 0
       then blank := i
       else if i > 1
       then (for j = i + 1 to size do
	       if contents.(i) > contents.(j) && contents.(j) <> 0 then
		 revord := !revord + 1
	     done))
    done;
    let e = 4 - (!blank / 4) in
      Verb.pe Verb.debug "%i inversions, %i blank row\n%!" !revord e;
      Math.divisible !revord 2


let inst ~nrows ~ncols initial goal =
  (** [inst ~nrows ~ncols initial goal] makes a new instance. *)
  let nelms = nrows * ncols in
    if (Array.length initial) <> nelms
    then invalid_arg "Initial configuration has incorrect number of tiles";
    if (Array.length goal) <> nelms
    then invalid_arg "Goal configuration has incorrect number of tiles";
    { nrows = nrows; ncols = ncols; nelms = nelms;
      glued = [];
      initial = initial; goal = goal }


let canonical_goal ~nrows ~ncols =
  (** [canonical_goal ~nrows ~ncols] creates the canonical goal
      configuration where each tile is in the location that matches
      its value. *)
  Array.init (nrows * ncols) Fn.identity


let read ?(read_glued = false) inch =
  (** [read inch] reads an instance from the given channel. *)
  let nrows = Wrio.input_int inch and ncols = Wrio.input_int inch in
  let nelms = nrows * ncols in
    Wrio.skip_through_str inch "tile:";
    let init = Wrarray.read_ints inch nelms in
      Wrio.skip_through_str inch "ns:";
      let goal = Wrarray.read_ints inch nelms in
      let glued = ref [] in
	if(read_glued) then
	  (
	    Wrio.skip_through_str inch "glued:";
	    glued := Wrio.read_ints inch;
	  );
	{ nrows = nrows; ncols = ncols; nelms = nelms;
	  glued = !glued;
	  initial = contents_of_positions init;
	  goal = contents_of_positions goal;
	}


let load file =
  (** [load file] loads the instance from a file. *)
  let inch = open_in file in
  let inst = read inch in
    close_in inch;
    inst


let write inst outch =
  (** [write inst outch] writes the given instance to an output
      channel. *)
  let ipositions = positions_of_contents inst.initial in
  let gpositions = positions_of_contents inst.goal in
    fprintf outch "%d %d\n" inst.nrows inst.ncols;
    fprintf outch "starting positions for each tile:\n";
    Array.iter (fprintf outch "%d\n") ipositions;
    fprintf outch "goal positions:\n";
    Array.iter (fprintf outch "%d\n") gpositions


let save inst file =
  (** [save inst file] saves the given instance to a file. *)
  Wrio.with_outfile file (write inst)


let rand ~nrows ~ncols =
  (** [rand ~nrows ~ncols] gets a random solvable instance. *)
  let pos = canonical_goal ~nrows ~ncols in
    ignore (Wrutils.eval_until (fun _ -> Wrarray.permute pos; pos) is_solvable);
    { nrows = nrows;
      ncols = ncols;
      nelms = nrows * ncols;
      glued = [];
      initial = pos;
      goal = canonical_goal ~nrows ~ncols; }


let make_random_set ?(overwrite=false) ~nrows ~ncols ~count =
  let inst_root = User_paths.instance_root ^ "tiles_instances/" in
  let set_attrs =
    [ "model", "random";
      "rows", string_of_int nrows;
      "cols", string_of_int ncols; ]
  in
    for i = 1 to count do
      let inst_attrs = ("num", string_of_int i) :: set_attrs in
      let file = Rdb.path_for inst_root inst_attrs in
	if overwrite || not (Sys.file_exists file)
	then (let inst = rand ~nrows ~ncols in
		Verb.pr Verb.always "Saving %s\n" file;
		save inst file)
	else Verb.pr Verb.always "Skipping %s\n" file
    done




(* EOF *)
