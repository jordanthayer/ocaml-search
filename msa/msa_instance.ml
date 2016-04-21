(* $Id: instance.ml,v 1.1 2006/06/30 19:43:09 ruml Exp $

   instances
*)


type t = string array

type sol = string array

let gap = '-'

let gap_cost = 2
let subst_cost = 1


let num_seqs i =
  Array.length i


let seq_len i j =
  String.length i.(j)


(******** making **********)


let random_seq len alphabet_size =
  let a = Char.code 'a' in
    Wrstr.init len (fun _ -> Char.chr (a + (Random.int alphabet_size)))


let similar_seq prob_same alphabet_size seq =
  let a = Char.code 'a' in
    Wrstr.map (fun c ->
		 if Math.true_with_prob prob_same then
		   c
		 else
		   Char.chr (a + (Random.int alphabet_size)))
      seq


let random seq_len num_seqs alphabet_size =
  Array.init num_seqs (fun _ -> random_seq seq_len alphabet_size)


let similar prob_same seq_len num_seqs alphabet_size =
  let master = random_seq seq_len alphabet_size in
    Array.of_list (master::
		     Wrutils.map_ntimes
		     (fun _ -> similar_seq prob_same alphabet_size master)
		     (num_seqs - 1))


let drop prob master =
  let buf = Buffer.create (String.length master) in
    String.iter (fun c ->
		   if not (Math.true_with_prob prob) then
		     Buffer.add_char buf c)
      master;
    Buffer.contents buf


let morphed prob_subst prob_del master_len num_seqs alphabet_size =
  (** prob_subst, prob_del *)
  let master = random_seq master_len alphabet_size in
    Array.of_list (Wrutils.map_ntimes
		     (fun _ ->
			drop prob_del (similar_seq (1. -. prob_subst)
					 alphabet_size master))
		     num_seqs)


(* optimal is 5 *)
let simple_korf = [| "acgtacgtacgt";
		     "atgtcgtcacgt" |]


let hohwald_three_1 = [| "agtta";
			 "agctg";
			 "gacag"; |]
			(* optimal is 44 *)
let hohwald_three_2 = [| "catttacggaatacggatat";
			 "atacagccagggaaattgaa";
			 "aacagcaggatattcctaat"; |]
			(* optimal is 17 *)
let hohwald_two = [| "catttacggaatacggatat";
		     "atacagccagggaaattgaa"; |]

(* optimal is 12
   t-acttcg
   t-ac--cg
   tgac--cg *)
let zhou_three = [| "tacttcg";
		    "taccg";
		    "tgaccg"; |]


let make seqs =
  (** returns an instance using strings from [seqs].  All fresh. *)
  (* check for gaps, pad short ones with gaps *)
  let n = snd (Wrarray.max_by String.length seqs) in
    Array.map (fun s ->
		 if String.contains s gap then
		   failwith "make: sequence already contains a gap!";
		 Wrstr.pad_right s n gap)
      seqs


let load_real_raw path =
  Wrio.with_infile path
    (fun ch ->
       (Array.init 5 (fun _ -> Wrio.non_comment_line ch)))


(************* checking ***************)


let score_pair a b =
  let score = ref 0 in
    Wrstr.iter2 (fun x y ->
		   if x = gap then
		     (if y <> gap then
			score := !score + gap_cost)
		   else if y = gap then
		     score := !score + gap_cost
		   else if x <> y then
		     score := !score + subst_cost)
      a b;
    !score


let score sol =
  let total = ref 0
  and k = Array.length sol in
    for i = 0 to k-2 do
      for j = i+1 to k-1 do
	total := !total + (score_pair sol.(i) sol.(j))
      done
    done;
    !total


let check_match i s =
  (** checks that sol string [s] has chars from instance string [i] *)
  let ii = ref 0 in
    String.iter (fun c ->
		   if c <> gap then
		     if c = i.[!ii] then
		       incr ii
		     else
		       failwith "sol string has extraneous non-gap char")
      s;
    if !ii <> String.length i then
      failwith "sol string ends before instance string"


let check instance sol =
  (* solution strings all same length *)
  if not (Wrarray.constant_p (Array.map String.length sol)) then
    failwith "sol vectors of differing length";
  (* all match corresponding instance string except for gaps *)
  Wrarray.iter2 check_match instance sol;
  (* not necessarily same char in column in every sol, due to substitutions *)
  (* not all gap *)
  for pos = 0 to (String.length sol.(0))-1 do
    if Wrarray.for_all (fun x -> x.[pos] = gap) sol then
      failwith "all strings have a gap at some position!"
  done


(* EOF *)
