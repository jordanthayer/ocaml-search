(*
  computes a perfect hash for a permutation array.
  Only works if the arrays to be hashed are all the same size (say n)
  and the array contains elements 1 to n.
*)


(*
  little helper function that swaps values in an array.
*)
let swap a i1 i2 = 
  let temp = a.(i1) in
    a.(i1)<- a.(i2);
    a.(i2)<- temp
     

(*
  calculates the inverse of an array.  Required to rank arrays.
*)
let invert_array a = 
  let len = Array.length a in 
  let a_inv = Array.make len a.(0) in
    for i = 0 to (len-1) do
      a_inv.(a.(i)) <- i;
    done;
    a_inv
     

(*
  Calculates the permutation rank of the specified array.
*)
let rank a = 
  let rec rank_helper a a_inv n = 
    if(n == 1) then 0 
    else
      (
	let s = a.(n-1) in
	  swap a (n-1) (a_inv.(n-1));
	  swap a_inv s (n-1);
	  s + (n * (rank_helper a a_inv (n-1)));
      )
  in rank_helper (Array.copy a) (invert_array a) (Array.length a)


(*
  Given a length, and an integer, generates the array that would
  create that rank.
*)
let derank len n = 
  let a = Array.make len 0 in
  for i = 0 to (len-1) do
    a.(i) <- i;
  done;
  let rec derank_helper remaining_len r a = 
    if(remaining_len > 0) then
      (
	swap a (remaining_len-1) (r mod remaining_len);
	derank_helper (remaining_len-1) (r/remaining_len) a
      ) in
    derank_helper (len) n a;
    a
