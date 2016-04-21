(*
  computes a perfect hash for a permutation array.
  Only works if the arrays to be hashed are all the same size (say n)
  and the array contains elements 1 to n.

  Ranking and unranking permutations in linear time

  Wendy Myrvold and Frank Ruskey
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
  Verb.pe Verb.always "Unranking %i\n%!" n;
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
    Verb.pe Verb.always "done\n%!";
    a


let test_rank ar =
  let ranked = rank ar in
  let unranked = derank (Array.length ar) ranked in
    ranked,unranked

(************************************************************)
(* Lexical permutation ranking.                             *)
(* From: ``Efficient Algorithms to Rank and Unrank          *)
(*         Permutations in Lexicograhpic Order'' -- Bonet   *)
(************************************************************)


let lex_rank pos =
  (** [lex_rank pos] computes a lexicographical permutation
      ranking of the positions array [pos] in O(n log n) time. *)
  let n = Array.length pos in
  let k = Math.ceil_int (Math.log2 (float n)) in
  let tree_size = (* 2 * n - 1 *) Math.int_exp 2 (k + 1) in
  let left_of i = 2 * i
  and parent_of i = i / 2 in
  let tree = Array.make (tree_size + 1) 0 in
    (* This over-allocates the tree by 1-element (elem 0) but it keeps
       the 1-based intexing the same as the paper. *)
  let rank = ref 0 in
    for i = 1 to n do
      let ctr = ref pos.(i - 1) in
      let node = ref ((Math.int_exp 2 k) + pos.(i - 1)) in
	for j = 1 to k do
	  let parent = parent_of !node in
	    if Math.odd_p !node
	    then ctr := !ctr - tree.(left_of parent);
	    tree.(!node) <- tree.(!node) + 1;
	    node := parent;
	done;
	tree.(!node) <- tree.(!node) + 1;
	rank := !rank * (n + 1 - i) + !ctr;
    done;
    !rank



let rec fact n =
  match n with
    | 0 -> 1
    | 1 -> 1
    | _ -> n * (fact (n-1))


let construct_d n rank =
  let d = Array.create n 0
  and f = ref 0 in
    for i = 0 to n-1
    do
      (let j = n - 1 - i in
	 d.(j) <-  (rank / ((fact !f))) mod (!f + 1);
	 f := !f + 1)
    done;
    d

let lex_unrank n rank =
  let d = construct_d n rank in
  let k = Math.ceil_int (Math.log2 (float n)) in
  let ret = Array.make n 0
  and t = Array.make (Math.int_exp 2 (k+1)) 0 in
    for i = 0 to k do
      (for j = 1 to (Math.int_exp 2 i) do
	 t.((Math.int_exp 2 i) + j - 1) <- 1 lsl (k - i)
       done)
    done;
    for i = 1 to n do
      (let digit = ref d.(i-1)
       and node = ref 1 in
	 for j = 1 to k do
	   (t.(!node) <- t.(!node) - 1;
	    node := !node lsl 1;
	    if !digit >= t.(!node)
	    then (digit := !digit - t.(!node);
		  node := !node + 1;))
	 done;
	 t.(!node) <- 0;
	 ret.(i-1) <- !node - (Math.int_exp 2 k))
    done;
    ret

let lex_rank_abst size pos =
  (** [lex_rank_abst size pos] computes a lexicographical permutation
      ranking of the abstract positions array [pos] in O(n log n) time
      assuming that the non-abstract vector is of length [size].

      This can be used for pattern databases.  If you have a 3-tile
      pattern database for the 15-puzzle, for example, you can get the
      rank of the abstraction by calling [lex_rank_abst 16 pos] where
      [pos] is the array with the positions of all tiles that have not
      been abstracted away.

      This routine is adapted from [lex_rank] above.  [lex_rank] is
      from Bonet's Figure 4.  This is from nothing, it was made up.
  *)
  let n = Array.length pos in
  let k = Math.ceil_int (Math.log2 (float size)) in
  let tree_size = Math.int_exp 2 (k + 1) in
  let left_of i = 2 * i
  and parent_of i = i / 2 in
  let tree = Array.make (tree_size + 1) 0 in
    (* This over-allocates the tree by 1-element (elem 0) but it keeps
       the 1-based intexing the same as the paper. *)
  let rank = ref 0 in
    for i = 1 to n do
      let ctr = ref pos.(i - 1) in
      let node = ref ((Math.int_exp 2 k) + pos.(i - 1)) in
	for j = 1 to k do
	  let parent = parent_of !node in
	    if Math.odd_p !node
	    then ctr := !ctr - tree.(left_of parent);
	    tree.(!node) <- tree.(!node) + 1;
	    node := parent;
	done;
	tree.(!node) <- tree.(!node) + 1;
	rank := !rank * (size + 1 - i) + !ctr;
    done;
    !rank


let lex_unrank_abst n rank =
  let d = construct_d n rank in
  let k = Math.ceil_int (Math.log2 (float n)) in
  let ret = Array.make n 0
  and t = Array.make (Math.int_exp 2 (k+1)) 0 in
    for i = 0 to k do
      (for j = 1 to (Math.int_exp 2 i) do
	 t.((Math.int_exp 2 i) + j - 1) <- 1 lsl (k - i)
       done)
    done;
    for i = 1 to n do
      (let digit = ref d.(i-1)
       and node = ref 1 in
	 for j = 1 to k do
	   (t.(!node) <- t.(!node) - 1;
	    node := !node lsl 1;
	    if !digit >= t.(!node)
	    then (digit := !digit - t.(!node);
		  node := !node + 1;))
	 done;
	 t.(!node) <- 0;
	 ret.(i-1) <- !node - (Math.int_exp 2 k))
    done;
    ret


let test_lex array =
  let lexed = lex_rank array in
  let unlexd = lex_unrank (Array.length array) lexed in
    lexed,unlexd


let test_lex_abst n array =
  let lexed = lex_rank_abst n array in
  let unlexd = lex_unrank_abst (Array.length array) lexed in
    lexed,unlexd


(************************************************************)
(* The following code use to be in structs/array_hasher.ml  *)
(************************************************************)

(*
  this returns a function for hashing an array of size a_cols where
  the values in the array are integers that range from 0 to a_rows.
*)
let hash_array_function size max_range =


  let a = Array.init size (fun _ ->
			     Array.init max_range (fun _ ->
						     Random.bits ()))
  in
    (fun x ->
       let hash = ref 0 in
	 for i = 0 to (size - 1) do
	   assert (x.(i) < Array.length a);
	   hash := !hash + ((a.(i)).((x.(i))));
	 done;
	 !hash)


(*thus function hashes arrays by adding them together.*)
let add_array_hash _ _ =
  (fun x ->
    let hash = ref 0 in
      for i = 0 to (Array.length x) - 1
      do
	hash := !hash + x.(i);
      done;
      abs(!hash))
