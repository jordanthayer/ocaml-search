(** A graph of a road map.

    @author eaburns
    @since 2010-07-22
*)

open Printf
open Float_ref
open Bigarray

type int_bigarray = (int, int_elt, c_layout) Array1.t

type t = {
  nnodes : int;
  total_arcs : int;
  xs : int_bigarray;
  ys : int_bigarray;
  arc_offs : int_bigarray;
  tgt_arcs : int_bigarray;
  wt_arcs : int_bigarray;
}


let x_coord map n =
  (** [x_coord t n] gets the x coordinate of the given node. *)
  map.xs.{n - 1}


let y_coord map n =
  (** [y_coord t n] gets the y coordinate of the given node. *)
  map.ys.{n - 1}


let bounds map =
  (** [bounds map] get the coordinates that are on the bounds of the
      map. *)
  let nnodes = map.nnodes in
  let xmin = ref max_int and xmax = ref min_int in
  let ymin = ref max_int and ymax = ref min_int in
    for n = 1 to nnodes do
      let x = x_coord map n and y = y_coord map n in
	if x < !xmin then xmin := x;
	if x > !xmax then xmax := x;
	if y < !ymin then ymin := y;
	if y > !ymax then ymax := y;
    done;
    !xmin, !xmax, !ymin, !ymax


let num_arcs map n =
  (** [num_arcs map n] gets the number of outgoing arcs. *)
  let offs = map.arc_offs in
    if n == map.nnodes
    then map.total_arcs - offs.{n - 1}
    else offs.{n} - offs.{n - 1}


let arcs map n =
  (** [arcs map n] gets the arcs for the given node. *)
  let l = ref [] in
  let narcs = num_arcs map n in
  let offs = map.arc_offs.{n - 1} in
  let tgts = map.tgt_arcs and wts = map.wt_arcs in
    for i = 0 to narcs - 1 do
      l := (tgts.{offs + i}, wts.{offs + i}) :: !l
    done;
    !l


let is_arc map s t =
  (** [is_arc map s t] test if there exists an arc from [s] to [t]. *)
  let tgts = map.tgt_arcs in
  let rec find_arc ind tgts narcs t =
    if narcs = 0
    then false
    else
      if tgts.{ind} = t
      then true
      else find_arc (ind + 1) tgts (narcs - 1) t
  in find_arc map.arc_offs.{s - 1} tgts (num_arcs map s) t


let arc_costs map s t =
  (** [arc_costs map s t] finds the cost of the given arc. *)
  let tgts = map.tgt_arcs in
  let wts = map.wt_arcs in
  let rec find_arc accum ind tgts narcs t =
    if narcs = 0
    then accum
    else
      let accum' = if tgts.{ind} = t then wts.{ind} :: accum else accum in
	find_arc accum' (ind + 1) tgts (narcs - 1) t
  in find_arc [] map.arc_offs.{s - 1} tgts (num_arcs map s) t


let nearest_node map ~x ~y =
  (** [nearest_node map ~x ~y] finds the node in the graph with the
      coordinates nearest to [x], [y]. *)
  let dist i j =
    let dx = x -. float i and dy = y -. float j in dx *. dx +. dy *. dy in
  let nearest_d = ref infinity in
  let nearest_n = ref ~-1 in
  let nnodes = map.nnodes in
    for n = 1 to nnodes do
      let i = x_coord map n and j = y_coord map n in
      let d = dist i j in
	if n = 1 || d < !nearest_d then begin
	  nearest_d := d;
	  nearest_n := n;
	end;
    done;
    !nearest_n, x_coord map !nearest_n, y_coord map !nearest_n, !nearest_d


module Binary_format = struct
  (*
    All fields are 4-byte big-endian (a.k.a network byte ordered):

    The header that appears at the beginning of the file is:

    +----+----+----+----+----+----+----+----+----+
    |ver | number of nodes   |  number of arcs   |
    +----+----+----+----+----+----+----+----+----+

    The current version is 2, so the first byte ('ver') should contain
    that value. Nodes are numbered 1 to 'number of nodes' since that
    is how they are in the DIMACS data.  The header is followed by a
    list of nodes.  The format for each node is the following:

    +----+----+----+----+----+----+----+----+----+----+----+----+
    |    node number    |    x coordinate   |   y coordinate    |
    +----+----+----+----+----+----+----+----+----+----+----+----+
    +----+----+----+----+ +----+----+----+----+----+----+----+----+...
    |  number of arcs   | |    target node    |       weight      |...
    +----+----+----+----+ +----+----+----+----+----+----+----+----+...
  *)


  let max_version = 2


  let check_version v =
    (** [check_version v] tests that this is a version that we are
	able to read. *)
    if v > max_version
    then failwith
      (sprintf "Unsupported version: %d, maximum supported version is %d"
	 v max_version)

  let read_node_header n inch =
    (** [read_node_header n inch] reads the header for a node. *)
    let num = input_binary_int inch in
      if num <> n + 1
      then failwith (sprintf "Expected node number %d, got %d" (n + 1) num);
      let x = input_binary_int inch in
      let y = input_binary_int inch in
      let narcs = input_binary_int inch in
	x, y, narcs


  let node_header_arrays nnodes =
    (** [node_header_arrays nnodes] create arrays for the nodes'
	header information. *)
    let xs = Array1.create int c_layout nnodes in
    let ys = Array1.create int c_layout nnodes in
    let narcs = Array1.create int c_layout nnodes in
      xs, ys, narcs

  let extend ary num =
    let len = Array1.dim ary in
    let ary' = Array1.create int c_layout (len + num) in
      for i = 0 to len - 1 do ary'.{i} <- ary.{i} done;
      ary'


  let make_ensure_size nnodes =
    (** [make_ensure_size nnodes] make a function that ensures the
	size of the big targets and weights arrays is large enough to
	add a desired number of arcs.  We also try to grow these
	intelligently based on the number of nodes remaining and the
	mean degree. *)
    let mean_deg = float_ref 0. and count = float_ref 0. in
    let update_mean narcs =
      count <-- !!count +. 1.;
      mean_deg <-- !!mean_deg +. (((float narcs) -. !!mean_deg) /. !!count);
    in
      (fun ~tgts ~wts fill narcs n ->
	 (* [n] is the array index of the current node. *)
	 let size = Array1.dim !tgts in
	   assert (size = (Array1.dim !wts));
	   update_mean narcs;
	   if fill + narcs >= size
	   then begin
	     let extra = (float (nnodes - n - 1)) *. !!mean_deg in
	     let nadd = narcs + (Math.round extra) in
	       tgts := extend !tgts nadd;
	       wts := extend !wts nadd;
	   end)


  let read_nodes_ver1 nnodes inch =
    (** [read_nodes_ver1 nnodes inch] reads the nodes from
	the file. *)
    let ensure_size = make_ensure_size nnodes in
    let xs, ys, narcs = node_header_arrays nnodes in
    let offs = Array1.create int c_layout nnodes in
    let tgts = ref (Array1.create int c_layout nnodes) in
    let wts = ref (Array1.create int c_layout nnodes) in
      offs.{0} <- 0;
      for n = 0 to nnodes - 1 do
	let o = offs.{n} in
	let x, y, na = read_node_header n inch in
	  ensure_size ~tgts ~wts o na n;
	  xs.{n} <- x;
	  ys.{n} <- y;
	  for i = 0 to na - 1 do
	    let ind = o + i in
	      !tgts.{ind} <- input_binary_int inch;
	      !wts.{ind} <- input_binary_int inch;
	  done;
	  narcs.{n} <- na;
	  if n < nnodes - 1 then offs.{n + 1} <- o + na;
      done;
      let total_arcs = offs.{nnodes - 1} + narcs.{nnodes - 1} in
	xs, ys, offs, narcs, !tgts, !wts, total_arcs


  let read_nodes_ver2 nnodes total_arcs inch =
    (** [read_nodes_ver2 nnodes total_arcs inch] reads the nodes from
	the file. *)
    let xs, ys, narcs = node_header_arrays nnodes in
    let offs = Array1.create int c_layout nnodes in
    let tgts = Array1.create int c_layout total_arcs in
    let wts = Array1.create int c_layout total_arcs in
      offs.{0} <- 0;
      for n = 0 to nnodes - 1 do
	let o = offs.{n} in
	let x, y, na = read_node_header n inch in
	  xs.{n} <- x;
	  ys.{n} <- y;
	  for i = 0 to na - 1 do
	    let ind = o + i in
	      tgts.{ind} <- input_binary_int inch;
	      wts.{ind} <- input_binary_int inch;
	  done;
	  narcs.{n} <- na;
	  if n < nnodes - 1 then offs.{n + 1} <- o + na;
      done;
      xs, ys, offs, narcs, tgts, wts


  let read inch =
    (** [read inch] reads the graph from the given file. *)
    let ver = input_byte inch in
      check_version ver;
      let nnodes = input_binary_int inch in
	Verb.pr Verb.debug "%d nodes\n" nnodes;
	let xs, ys, offs, narcs, tgts, wts, total_arcs =
	  match ver with
	    | 1 ->
		read_nodes_ver1 nnodes inch
	    | 2 ->
		let total_arcs = input_binary_int inch in
		  Verb.pr Verb.debug "%d total arcs\n" total_arcs;
		  let xs, ys, offs, narcs, tgts, wts =
		    read_nodes_ver2 nnodes total_arcs inch
		  in xs, ys, offs, narcs, tgts, wts, total_arcs
	    | _ -> invalid_arg "Bad version"
	in
	  { nnodes = nnodes; total_arcs = total_arcs; xs = xs; ys = ys;
	    arc_offs = offs; tgt_arcs = tgts; wt_arcs = wts; }


  let load file =
    (** [load file] loads the roadmap from the given file. *)
    let inch = open_in file in
    let map = read inch in
      close_in inch;
      map


  let write_header map outch =
    (** [write_header map outch] writes the header to the channel. *)
    output_byte outch max_version;
    output_binary_int outch map.nnodes;
    output_binary_int outch (Array1.dim map.tgt_arcs)


  let write_node map n outch =
    (** [write_node map n outch] writes the node to the channel. *)
    output_binary_int outch n;
    output_binary_int outch (x_coord map n);
    output_binary_int outch (y_coord map n);
    output_binary_int outch (num_arcs map n);
    List.iter (fun (t, w) ->
		 output_binary_int outch t;
		 output_binary_int outch w;)
      (arcs map n)


  let write_nodes map outch =
    (** [write_nodes map outch] writes all of the nodes out to the
	channel. *)
    for n = 1 to map.nnodes do
      write_node map n outch;
    done


  let write map outch =
    (** [write map outch] writes the map to the given channel. *)
    write_header map outch;
    write_nodes map outch


  let save map file =
    (** [save map file] saves the roadmap to the given file. *)
    let outch = open_out file in
      write map outch;
      close_out outch

end

