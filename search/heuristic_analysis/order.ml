

type 'a n = 
{
  node:'a option;
  h_star:float;
  h:float;
}

let make_n n h_star h =
{
  node=n;
  h_star= h_star;
  h=h;
}

let fp_compare f1 f2 = 
  let del = f1 -. f2 in
  if(del > 0.0) then 1
  else if (del < 0.0) then (-1)
  else 0

let check_order (items:'a n list) = 
  let arr = Array.of_list (List.sort (fun a b -> fp_compare a.h b.h) items) in
  let inversion_count = ref 0 in
    for i = 0 to (Array.length arr)- 1 
    do
      (*check items after for smaller ones*)
      for j = i + 1 to (Array.length arr) - 1 
      do
	if((fp_compare arr.(i).h_star arr.(j).h_star) = 1) then
	  inversion_count := !inversion_count + 1;
      done;
      
    done;
    !inversion_count


let check_beam_order (items:'a n list) (bf:float) = 
  let arr = Array.of_list (List.sort (fun a b -> fp_compare a.h b.h) items) in
  let inversion_count = ref 0 in
    for i = 0 to (Array.length arr)- 1 
    do
      (*check items after for smaller ones*)
      for j = i + 1 to (Array.length arr) - 1 
      do
	if((fp_compare arr.(i).h_star arr.(j).h_star) = 1) then
	  inversion_count := !inversion_count + 1;
      done;
      
    done;
    !inversion_count


let check_order_p (items:'a n list) = 
  let arr = Array.of_list (List.sort (fun a b -> fp_compare a.h b.h) items) in
  let inversion_count = ref 0.0 in
    for i = 0 to (Array.length arr)- 1 
    do
      (*check items after for smaller ones*)
      for j = i + 1 to (Array.length arr) - 1 
      do
	if((fp_compare arr.(i).h_star arr.(j).h_star) = 1) then
	  inversion_count := !inversion_count +. 1.0;
      done;
    done;
    let den = (float_of_int (Math.n_choose_k (Array.length arr) 2)) in
      !inversion_count /. den
