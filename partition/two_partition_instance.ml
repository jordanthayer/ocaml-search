(** An instance of a two partition problem is just an array of big_ints.

    Instances are guaranteed to be sorted in descending order.

    @author eaburns
    @since 2009-12-29
*)

open Big_int


type t = big_int array
    (* The numbers in the instance (sorted from largest to smallest). *)


let rec has_dups = function
    (** [has_dups lst] a clumbsy duplicate checking function. *)
  | [] -> false
  | hd :: tl -> (List.exists (eq_big_int hd) tl) || has_dups tl


let of_list ints =
  (** [of_list ints] gets the instance from a list of big ints. *)
  if (has_dups ints)
  then invalid_arg "duplicated numbers";
  let ary = Array.of_list ints in
    Array.sort (fun a b -> compare_big_int b a) ary;
    ary


(************* I/O ***************)


let read ch =
  let n = Wrio.input_int ch in
    of_list (Wrutils.map_ntimes
	       (fun () -> big_int_of_string (input_line ch))
	       n)


let write ch i =
  Wrutils.pf ch "%d\n" (Array.length i);
  Array.iter (fun x -> Wrio.output_line ch (string_of_big_int x)) i


let save_instance overwrite model digits size inst_num inst =
  (** [save_instance overwrite model digits count inst_num inst] saves
      an instance to the group data RDB.  [digits] is the number of
      digits in each number.  [size] is the number of numbers to
      partition [inst_num] is the instance number to use. *)
  let attrs = [ "model", model;
		"digits", string_of_int digits;
		"size", string_of_int size;
		"num", string_of_int inst_num;
	      ] in
  let path = Rdb.path_for (User_paths.instance_root ^ "partition") attrs in
    if overwrite || not (Sys.file_exists path)
    then begin
      Verb.pr Verb.often "Saving %s\n" path;
      Wrio.with_outfile path (fun out -> write out inst)
    end else
      Verb.pr Verb.often "Skipping %s\n" path


let random_big_int digits =
  (** [random_big_int digits] gets a uniform random big integer of the
      given number of digits. *)
  let n = ref zero_big_int in
    Wrutils.ntimes (fun () ->
		      n := (add_int_big_int
			      (Random.int 10)
			      (mult_int_big_int 10 !n)))
      digits;
    !n


let digits_for_prob size prob =
  (** [digits_for_prob size prob] gets the number of digits that will
      be required for a problem with [size] numbers to give a
      probability of [prob] of having a perfect partition. *)
  let n = float size
  and pi = Math.pi
  in truncate (ceil (~-. (log10 ((prob /. (2. ** n)) *. (sqrt (n /. (24. *. pi)))))))


(************* Korf-like instances ***************)


let rec make_korf size =
  (** [make_korf size] makes a "Korf" instance with [size] numbers
      each with 10 digits. *)
  let lst = Wrutils.map_ntimes (fun () -> random_big_int 10) size in
    if has_dups lst then make_korf size else of_list lst


let korf_sizes =
  (** [korf_sizes] a lazily created list counting from 5 to 100 by
      5's.  These are the sizes that Korf uses in the CKK paper. *)
  lazy (let rec make_korf_sizes n =
	  if n = 100
	  then [100]
	  else n :: make_korf_sizes (n + 5)
	in make_korf_sizes 5)


let make_korf_set n =
  (** [make_korf_set n] makes [n] Korf instances of each of Korf's
      sizes and saves them in the group data RDB. *)
  List.iter (fun size ->
	       for i = 1 to n do
		 save_instance false "korf" 10 size i (make_korf size)
	       done)
    (Lazy.force korf_sizes)


(************* Wheeler-ish instances ***************)

let wheeler_perfect_prob = 1e-5
  (** [wheeler_perfect_prob] the probability of a perfect partition
      to use for Wheeler like instances (he claims to have copied this
      probability from Karmarkar et al.). *)

let rec make_wheeler size =
  (** [make_wheeler size] makes a "Wheeler" instance with [size]
      numbers.  The result is the number of digits and the instance in a
      tuple. *)
  let digits = digits_for_prob size wheeler_perfect_prob in
  let lst = Wrutils.map_ntimes (fun () -> random_big_int digits) size in
    if has_dups lst then make_wheeler size else digits, of_list lst


let wheeler_sizes = [ 32; 64; 128; 256; 512; ]
  (** [wheeler_sizes] is a list of the sizes that Wheeler uses for his
      number partitioning problems. *)


let make_wheeler_set n =
  (** [make_wheeler_set n] makes [n] Wheeler instances of each of
      Wheeler's sizes and saves them in the group data RDB. *)
  List.iter (fun size ->
	       for i = 1 to n do
		 let digits, inst = make_wheeler size in
		   save_instance false "wheeler" digits size i inst
	       done)
    wheeler_sizes
