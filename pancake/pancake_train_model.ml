(** Training a CDP model on pancakes.

    @author eaburns
    @since 2010-10-19
*)

open Printf

let make_cdp_next ~ncakes ~num =
  let branching = float (ncakes - 2) in
    (* branching is number of cakes minus the parent and the 1-cake
       flip. *)
  let remaining = ref num in
  let queue = Queue.create () in
  let populate () =
    Pancake_sampling.sample ~ncakes
      (fun gp ->
	 let gph = truncate gp.Pancake_sampling.h in
	   List.iter
	     (fun p ->
		let ph = truncate p.Pancake_sampling.h in
		  List.iter (fun c ->
			       Queue.push (branching,
					   truncate c.Pancake_sampling.h, 0,
					   ph, 0,
					   gph, 0)
				 queue;)
		    p.Pancake_sampling.successors)
	     gp.Pancake_sampling.successors)
      ~ancestors:2
      ~num:1;
  in
  let next () =
    if Queue.is_empty queue then populate ();
    Queue.pop queue
  in
    (fun () ->
       if !remaining = 0 then raise End_of_file;
       decr remaining;
       if Math.divisible !remaining 1_000_000 then
	 Printf.printf "%d remaining\n%!" !remaining;
       next ())


let train_cdp ~ncakes ~num =
  let next = make_cdp_next ~ncakes ~num in
  let hmax = ncakes in
  let ntypes = 1 in
    Gkre.learn hmax ntypes next


let train_im bins ~ncakes ~num () =
  let model = Im.make bins in
  let get_d n = truncate n.Pancake_sampling.h
  and get_t _ = 0
  and get_f n = n.Pancake_sampling.g -. 1. +. n.Pancake_sampling.h in
  let train = Im.make_train model get_t get_d get_f in
  let parents = ref 0 in
    begin try
      Pancake_sampling.sample ~ncakes
	(fun gp ->
	   List.iter (fun parent ->
			incr parents;
			if !parents > num then raise End_of_file;
			if (!parents mod 1_000_000) = 0
			then
			  Printf.printf "%d parents remaining\n%!"
			    (num - !parents);
			let kids =
			  List.filter
			    (fun k ->
			       k.Pancake_sampling.contents
			       <> gp.Pancake_sampling.contents)
			    parent.Pancake_sampling.successors
			in train 0 parent kids)
	     gp.Pancake_sampling.successors)
	~ancestors:2 ~num;
    with End_of_file -> ()
    end;
    model

let main () =
  let num = ref 1_000_000 in
  let ncakes = ref 14 in
  let outfile = ref "out.cdp" in
  let im = ref false in
    Arg.parse [
      "--im", Arg.Set im, "Use the incremental model, not CDP";

      "--num", Arg.Set_int num,
      sprintf "number of samples (default: %d)" !num;

      "--ncakes", Arg.Set_int ncakes,
      sprintf "number of cakes (default: %d)" !ncakes;
    ] (fun s -> outfile := s) "";
    if !im then begin
      Printf.printf
	"Training an incremental model with %d samples on %d cakes\n%!"
	!num !ncakes;
      let im = train_im 100 ~ncakes:!ncakes ~num:!num () in
	Printf.printf "saving to %s\n%!" !outfile;
	Wrio.with_outfile !outfile
	  (fun outch -> Marshal.to_channel outch im [])
    end else begin
      Printf.printf "Training a CDP model with %d samples on %d cakes\n%!"
	!num !ncakes;
      let cdp = train_cdp ~ncakes:!ncakes ~num:!num in
	Printf.printf "saving to %s\n" !outfile;
	Wrio.with_outfile !outfile
	  (fun outch -> Marshal.to_channel outch cdp [])
    end

let _ = main ()
