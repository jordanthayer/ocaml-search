(** Create a bunch of random trees in the graph and take the mean
    subtree size values beneath the trees.  The advantage is that the
    trees should be quick to generate, however, we may get wildly
    inaccurate f values.

    @author eaburns
    @since 2010-07-19
*)

type average = {
  mutable avg : float;
  mutable count : float;
}

let rec random_tree nodes seen info key h expand f_max depth cost root =
  (** [random_tree nodes seen info key h expand f_max depth cost root]
      Builds a random tree. *)
  let k = key root in
    if ((h root) +. cost) < f_max
      && not (Htable.mem seen k)
      && not (Limit.halt_p info)
    then begin
      let kids = Array.of_list (expand root cost) in
	Htable.add seen k true;
	Limit.incr_exp info;
	Limit.incr_gen_n info (Array.length kids);
	Wrarray.permute kids;
	let tree_size, max_depth =
	  Array.fold_left
	    (fun (s, d) (c, g) ->
	       let size, md =
		 random_tree nodes seen info key h expand f_max
		   (depth + 1) g c
	       in s +. size, max md d)
	    (1., depth) kids
	in
	  if not (Limit.halt_p info)
	    (* Don't count the final tree because it was probably cut
	       short. *)
	  then begin
	    try
	      let n = Htable.find nodes k in
	      let avg = n.avg in
		n.count <- n.count +. 1.;
		n.avg <- avg +. ((tree_size -. avg) /. n.count)
	    with Not_found ->
	      let n = { avg = tree_size ; count = 1. } in
		Htable.add nodes k n
	  end;
	  tree_size, max_depth
    end else 0., depth


let estimate_size nodes key root =
  (** [estimate_size nodes key root] estimates the size of the subtree
      under the root by merely looking at the average subtree size. *)
  try
    let n = Htable.find nodes (key root) in
      n.avg
  with Not_found ->
    failwith "Never expanded the root node."



let dups sface argv =
  (** [dups sface argv] Prefroms a random trees estimation in a domain
      with duplicates. *)
  let f_max = Search_args.get_float "random_trees" argv 0 in
  let expand = sface.Search_interface.domain_expand
  and key = sface.Search_interface.key
  and hash = sface.Search_interface.hash
  and eq = sface.Search_interface.equals
  and h = sface.Search_interface.h
  and info = sface.Search_interface.info
  and root = sface.Search_interface.initial in
  let nodes = Htable.create hash eq 100
  and seen = Htable.create hash eq 100 in
  let count = ref 0 and max_depth = ref 0 in
    while not (Limit.halt_p info) do
      Htable.clear seen;
      let _, d = random_tree nodes seen info key h expand f_max 0 0. root in
	max_depth := max d !max_depth;
	incr count;
    done;
    let size = estimate_size nodes key root in
      Datafile.write_pairs stdout
	[
	  "maximum f", string_of_float f_max;
	  "final estimation", string_of_float size;
	  "number of trees", string_of_int !count;
	  "max depth", string_of_int !max_depth;
	  "unique states", string_of_int (Htable.length nodes);
	];
      Limit.unwrap_sol6
	(function
	   | Limit.Nothing -> None
	   | Limit.Incumbent (f, state) -> Some (state, f))
	(Limit.results6 info)


