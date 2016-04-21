(** A (kind of) inefficient roadmap representation that is convenient
    for reading from the DIMACS style data files.

    @author eaburns
    @since 2010-08-18
*)

open Printf
open Bigarray

let rec dimacs_next_uncommented_line inch =
  (** [dimacs_next_uncommented_line inch] gets the next uncommented
      line from a DIMACS file. *)
  let line = input_line inch in
    if line.[0] = 'c'
    then dimacs_next_uncommented_line inch
    else line

(* The DIMACS data files are split up into graph and coordinate
   files and so this struct has types to store each individule file
   type reasonably efficiently in memory.  Additionally, this format
   uses arc lists instead of more tightly packed arrays because,
   when reading DIMACS, you don't know the number of out going arcs
   per node.  If you are storing graphs and coordinates, however,
   this as not the most efficient format. *)

type graph = {
  narcs : int;
  g_nnodes : int;
  nodes : arcs array;
}

and arcs = Nil | Arc of int * int * arcs
  (* Should be a tiny bit faster and more space efficient than using
     an [int * int list] *)


let read_graph inch =
  (** [read_graph inch] reads a graph in the DIMACS
      format. *)
  let prob_line = dimacs_next_uncommented_line inch in
  let nnodes, narcs =
    Scanf.sscanf prob_line "p sp %d %d" (fun n a -> n, a) in
  let nodes = Array.create nnodes Nil in
    Verb.pr Verb.toplvl "%d nodes and %d arcs\n" nnodes narcs;
    try
      while true do
	let line = dimacs_next_uncommented_line inch in
	  Scanf.sscanf line "a %d %d %d"
	    (fun t h w ->
	       let n = h - 1 in
		 nodes.(n) <- Arc (t, w, nodes.(n)));
      done;
      failwith "Cannot reach here";
    with End_of_file ->
      { g_nnodes = nnodes; narcs = narcs; nodes = nodes; }


let load_graph file =
  (** [load_graph file] loads a graph in DIMACS format from
      the given file. *)
  let inch = open_in file in
  let g = read_graph inch in
    close_in inch;
    g


let iter_successors gr n f =
  (** [iter_successors gr n f] iterates [f] over the successors of
      the node [n].  [f] must be of the type [tail:int -> weight:int
      -> unit]. *)
  let rec iter = function
    | Nil -> ()
    | Arc (tail, weight, arcs) -> f tail weight; iter arcs
  in iter gr.nodes.(n - 1)


type coordinates = {
  c_nnodes : int;
  xs : int array;
  ys : int array;
}

let read_coordinates inch =
  (** [read_coordinates inch] reads the coordinates in the
      DIMACS format. *)
  let prob_line = dimacs_next_uncommented_line inch in
  let nnodes = Scanf.sscanf prob_line "p aux sp co %d" Fn.identity in
  let xs = Array.create nnodes max_int
  and ys = Array.create nnodes max_int in
  let nset = ref 0 in
    Verb.pr Verb.toplvl "%d nodes\n" nnodes;
    try
      while true do
	let line = dimacs_next_uncommented_line inch in
	  Scanf.sscanf line "v %d %d %d"
	    (fun v x y ->
	       let n = v - 1 in
		 xs.(n) <- x; ys.(n) <- y);
	  incr nset;
      done;
      failwith "Cannot reach here"
    with End_of_file ->
      if !nset < nnodes
      then Wrutils.pr "Warning: not all nodes were given a coordinate\n";
      { c_nnodes = nnodes; xs = xs; ys = ys; }


let load_coordinates file =
  (** [load_coordinates inch] loads the coordinates from a
      DIMACS coordinate file. *)
  let inch = open_in file in
  let c = read_coordinates inch in
    close_in inch;
    c


let x_coord co n = co.xs.(n - 1)
  (** [x_coord co n] gets the x-coordinate of the node [n]. *)


let y_coord co n = co.ys.(n - 1)
  (** [y_coord co n] gets the y-coordinate of the node [n]. *)


let bounds co =
  (** [bounds co] finds the upper and lower bounds on x and y. *)
  let nnodes = co.c_nnodes in
  let xmin = ref max_int in
  let xmax = ref min_int in
  let ymin = ref max_int in
  let ymax = ref min_int in
    for n = 1 to nnodes do
      let i = x_coord co n in
      let j = y_coord co n in
	if i < !xmin then xmin := i;
	if i > !xmax then xmax := i;
	if j < !ymin then ymin := j;
	if j > !ymax then ymax := j;
    done;
    !xmin, !xmax, !ymin, !ymax



let closest_point coords ~x ~y =
  (** [closest_point coords ~x ~y] makes a KD tree ont the
      coordinates. *)
  let sq_dist x' y' =
    let xdiff = x - x' and ydiff = y - y' in
      xdiff * xdiff + ydiff * ydiff
  in
  let closest_dist = ref max_int in
  let closest_num = ref ~-1 in
  let xs = coords.xs and ys = coords.ys in
  let nnodes = coords.c_nnodes in
    for n = 0 to nnodes - 1 do
      let dist = sq_dist xs.(n) ys.(n) in
	if dist < !closest_dist || !closest_num < 0
	then begin
	  closest_dist := dist;
	  closest_num := n;
	end
    done;
    let cx = xs.(!closest_num) in
    let cy = ys.(!closest_num) in
      !closest_num + 1, cx, cy, !closest_dist


let coord_of_lon_lat ~lon ~lat =
  (** [coord_of_lon_lat ~lon ~lat] gets the coordinate associated with
      the given longitude and latitude. *)
  lon *. 1_000_000., lat *. 1_000_000.


module Binary_format = struct

  let read_node xs ys wts arc_count inch =
    (** [read_node xs ys wts arc_count inch] reads the given node from
	the input channel. *)
    let num = input_binary_int inch in
    let x = input_binary_int inch in
    let y = input_binary_int inch in
    let narcs = input_binary_int inch in
    let ind = num - 1 in
      arc_count := !arc_count + narcs;
      xs.(ind) <- x;
      ys.(ind) <- y;
      for i = 1 to narcs do
	let target = input_binary_int inch in
	let wt = input_binary_int inch in
	  wts.(ind) <- Arc(target, wt, wts.(ind));
      done

  let read inch =
    (** [read inch] reads a binary-stored roadmap from the given input
	channel.  Since the binary has all of the data (coordinates,
	travel time and distances) it is all read at once. *)
    let ver = input_byte inch in
      if ver > 2 then failwith "Unsupported version, max is 2";
      let nnodes = input_binary_int inch in
      let _ = if ver > 1 then input_binary_int inch else min_int in
      let narcs = ref 0 in
      let xs = Array.create nnodes min_int in
      let ys = Array.create nnodes min_int in
      let wts = Array.create nnodes Nil in
	for i = 1 to nnodes do
	  read_node xs ys wts narcs inch
	done;
	let coords = { c_nnodes = nnodes;
		       xs = xs;
		       ys = ys; } in
	let graph = { g_nnodes = nnodes;
		      narcs = !narcs;
		      nodes = wts; }
	in coords, graph


  let load file =
    (** [load file] loads a roadmap from the binary stored file into
	the DIMACS style data structures. *)
    let inch = open_in file in
    let (_,g) as res , time = Wrsys.with_time (fun () -> read inch) in
      close_in inch;
      Verb.pr Verb.optional "loaded %d nodes and %d arcs in %f seconds\n"
	(g.g_nnodes) (g.narcs) time;
      res


  let read_coordinates inch =
    (** [read_coordinates inch] reads just the coordinate data. *)
    let ver = input_byte inch in
      if ver > 2 then failwith "Unsupported version, max is 2";
      let nnodes = input_binary_int inch in
      let _ = if ver > 1 then input_binary_int inch else min_int in
      let xs = Array.create nnodes min_int in
      let ys = Array.create nnodes min_int in
	for i = 1 to nnodes do
	  let num = input_binary_int inch in
	  let x = input_binary_int inch in
	  let y = input_binary_int inch in
	  let narcs = input_binary_int inch in
	  let ind = num - 1 in
	    xs.(ind) <- x;
	    ys.(ind) <- y;
	    for j = 1 to narcs do
	      ignore (input_binary_int inch);
	      ignore (input_binary_int inch);
	    done
	done;
	{ c_nnodes = nnodes;
	  xs = xs;
	  ys = ys; }


  let load_coordinates file =
    (** [load_coordinates file] just load the coordinate data. *)
    let inch = open_in file in
    let c, time = Wrsys.with_time (fun () -> read_coordinates inch) in
      close_in inch;
      Verb.pr Verb.optional "loaded %d nodes in %f seconds\n"
	(c.c_nnodes) time;
      c
end


let to_roadmap c g =
  (** [to_roadmap c g] makes a [Roadmap.t] from the DIMACS format
      data. *)
  let nnodes = c.c_nnodes and total_arcs = g.narcs in
  let xs = Array1.create int c_layout nnodes in
  let ys = Array1.create int c_layout nnodes in
  let offs = Array1.create int c_layout nnodes in
  let tgts = Array1.create int c_layout total_arcs in
  let wts = Array1.create int c_layout total_arcs in
  let off = ref 0 in
  let na = ref 0 in
    for n = 1 to nnodes do
      let x = x_coord c n and y = y_coord c n in
      let o = !off in
	na := 0;
	xs.{n - 1} <- x;
	ys.{n - 1} <- y;
	offs.{n - 1} <- o;
	iter_successors g n (fun t w ->
			       let i = o + !na in
				 tgts.{i} <- t;
				 wts.{i} <- w;
				 incr na;);
	off := !off + !na;
    done;
    { Roadmap.nnodes = nnodes; Roadmap.total_arcs = total_arcs;
      Roadmap.xs = xs; Roadmap.ys = ys; Roadmap.arc_offs = offs;
      Roadmap.tgt_arcs = tgts; Roadmap.wt_arcs = wts }
