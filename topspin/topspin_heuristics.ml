(**

   heuristics for topspin problems

*)


let basic_h_a s = 
  let to_return = ref 0 in
    for i = 1 to (Array.length s.Topspin.disks) -1 
    do
      if(s.Topspin.disks.(0) != ((s.Topspin.disks.(i) + 
				    ((Array.length s.Topspin.disks) -
				       i +
				       (Array.length s.Topspin.disks)
				    )) mod
				   (Array.length s.Topspin.disks)))
      then 
	to_return := !to_return + 1;
    done;
    (float_of_int !to_return)


let basic_h_b s = 
  let to_return = ref 0 in 
    for i = 0 to (Array.length s.Topspin.disks) - 2
    do
      (*check if disk i is adjacent to disk i+1*)
      if(abs (s.Topspin.disks.(i) - s.Topspin.disks.(i+1)) = 1)
      then ()
      else
	to_return := !to_return + 1;
    done;
    if(abs (s.Topspin.disks.(0) - 
	      s.Topspin.disks.((Array.length s.Topspin.disks) - 1)) = 1)
    then ()
    else
      to_return := !to_return + 1;

    float_of_int !to_return


let basic_h_ab s = 
  (basic_h_a s) +. (basic_h_b s)


let bad_h s = 
  if(Topspin.is_goal s)
  then 0.0
  else 1.0
