(** Sampling a tree.  Code for randomly probing a tree and saving the
    trajectories of the probes along with their leaves.

    @author eaburns
    @since 2010-01-15
*)

type t = sample array

and sample = {
  trajectory : char array;
  leaf_cost : float;
}

let random_child max_children = (Random.int 100) mod max_children

let rec probe is_leaf leaf_cost num_children nth_child trajectory node =
  (** [probe is_leaf leaf_cost num_children nth_child trajectory node]
      makes a random probe in the search space and returns a
      sample. *)
  if is_leaf node
  then begin
    {
      trajectory = Array.of_list (List.rev trajectory);
      leaf_cost = leaf_cost node;
    }
  end else begin
    let nchildren = num_children node in
    let rank = random_child nchildren in
    let r = char_of_int rank in
    let child = nth_child node rank in
      probe is_leaf leaf_cost num_children nth_child (r::trajectory) child
  end


let run is_leaf leaf_cost num_chidlren nth_child root num =
  (** [run is_leaf leaf_cost num_chidlren nth_child root num] does
      [num] probes in the search space and returns an array of the
      samples that were gathered. *)
  let probe = probe is_leaf leaf_cost num_chidlren nth_child in
  let ary = Array.make num { trajectory = [| |]; leaf_cost = nan } in
    Verb.pr Verb.often "\n";
    for i = 0 to num - 1 do
      Verb.pr Verb.often "remaining: %-10d\r%!" (num - i);
      ary.(i) <- probe [] root;
    done;
    Verb.pr Verb.often "remaining: %-10d\n" 0;
    ary


let save ary path =
  (** [save ary path] saves an array of samples to the given file. *)
  Wrio.with_outfile path (fun out -> Marshal.to_channel out ary [])


let restore path =
  (** [restore path] restores an array from a file. *)
  let ch = open_in path in
  let ary = (Marshal.from_channel ch : sample array) in
    close_in ch;
    ary


let to_separate max_depth max_children ary =
  (** [to_separate max_depth max_children ary] converts an array of
      samples to an array of separate model samples and runs a
      regression on them to get a separate model. *)
  let nsamples = Array.length ary in
  let nfeatures = max_depth * max_children in
  let patterns = Array.make nsamples [| |]
  and targets = Array.make nsamples 0. in
    if nfeatures > nsamples then invalid_arg "to_separate: too few samples";
    for i = 0 to nsamples - 1 do
      let sample = ary.(i) in
      let trajectory = sample.trajectory in
      let sample_depth = (Array.length trajectory) - 1 in
      let pattern = Array.make nfeatures 0. in
	for d = 0 to sample_depth do
	  let r = int_of_char trajectory.(d) in
	    pattern.(d * max_children + r) <- 1.;
	done;
	patterns.(i) <- pattern;
	targets.(i) <- sample.leaf_cost;
    done;
    Offline_lms.lms patterns targets
