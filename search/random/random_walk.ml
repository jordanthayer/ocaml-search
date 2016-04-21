(**

   Randomly walks until the specified number of steps have been taken
   then just stop.

*)


let random_walk sif args =
  (**does a random walk using sif.  args says how many steps to take.*)
  (*shouldn't have any arguments.*)
  let n_steps = Search_args.get_int "random_walk" args 0 in
  let rec do_random_walk s count = 
    if(count = 0) then s
    else
      (
	let children = fst (List.split (sif.Search_interface.domain_expand s 0.0)) in
	let chosen = Wrlist.random_elt children in
	  do_random_walk chosen (count - 1)
      ) in
  let to_return = do_random_walk sif.Search_interface.initial n_steps in

    (to_return, 0.0), 0,0,0,0


let extract_result ((r,_),_,_,_,_)=
  r

