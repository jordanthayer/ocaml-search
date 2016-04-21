(** Knuth's sampling method applied to search graphs.

    @author eaburns
    @since 2010-06-08
*)

open Num

let zero_num = num_of_int 0
let one_num = num_of_int 1

type 'state node = {
  state : 'state;
  f : float;
}

let rec sample info see_state expand h f_max ?(branching=one_num)
    ?(cost=0.) root =
  (** [sample info see_state expand f_max ?branching ?cost root] makes
      a sample trajectory in the graph and estimates the number of
      nodes. *)
  if Limit.halt_p info
  then zero_num
  else begin
    if root.f >= f_max
    then zero_num
    else begin
      see_state root.state;
      let children = expand root.state cost in
      let nchildren = List.length children in
	if nchildren > 0
	then begin
	  let the_one = Random.int nchildren in
	  let branching' = (num_of_int nchildren) */ branching in
	  let child, cost' = List.nth children the_one in
	  let root' = { state = child; f = cost' +. (h child) } in
	    Limit.incr_exp info;
	    Limit.incr_gen_n info nchildren;
	    let ch =
	      sample info see_state expand h f_max ~branching:branching'
		~cost:cost' root'
	    in
	      branching +/ ch
	end else branching

    end
  end


let do_samples info see_state expand h f_max root =
  (** [do_samples info see_state expand h f_max root] perform samples
      until the limit is reached.  The result is an estimate of the
      number of nodes along with the number of samples.

      [see_state] is called for each node expansion with the state of the
      node that was expanded. *)
  let root_node = { state = root; f = h root } in
  let avg = ref zero_num and count = ref zero_num in
    while not (Limit.halt_p info) do
      let est = sample info see_state expand h f_max root_node in
	if not (Limit.halt_p info)
	then begin
	  let count' = !count +/ one_num in
	    avg := !avg +/ ((est -/ !avg) // count');
	    count := count';
	end
    done;
    if !count >/ zero_num then !avg, !count else one_num, zero_num


let dups sface argv =
  (** [dups sface argv] makes a function that estimates in a
      duplicates domain. *)
  let f_max = Search_args.get_float "knuth_sampling" argv 0 in
  let expand = sface.Search_interface.domain_expand
  and h = sface.Search_interface.h
  and info = sface.Search_interface.info
  and root = sface.Search_interface.initial in
  let estimate, nsamples =
    do_samples info (fun _ -> ()) expand h f_max root
  in
    Datafile.write_pairs stdout
      [ "num samples", string_of_int (int_of_num nsamples);
	"maximum f", string_of_float f_max;
	"final estimation", string_of_float (float_of_num estimate);
      ];
    Limit.unwrap_sol6
      (function
	 | Limit.Nothing -> None
	 | Limit.Incumbent (f, state) -> Some (state, f))
      (Limit.results6 info)
