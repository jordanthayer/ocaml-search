(* $Id: stn.ml,v 1.2 2004/11/11 17:53:26 ruml Exp $

   stn with destructive updates

   constraints can be between any two time points, but data structure only
   tracks bounds relative to Stn.zero, so all queries must be related to
   that point.  Fast, but can't answer questions about relations between
   arbitrary points.

   based on Cervoni, Cesta, and Oddi, "Managing Dynamic Temporal Constraint
   Networks", AIPS-94, pp13--? and Cesta and Oddi, "Gaining Efficiency and
   Flexibility in the Simple Temporal Problem", TIME-96

   Currently, we only add constraints that change the current bounds.  This
   will need to change if we allow constraint removals later.


   NOTE: By removing "dead" or "past" time points and their constraints,
   we need to make sure that we will never remove a time point t_i,
   assign that id to another time point and having a program refer
   to the "old" t_i and the STN think that it's refer to the new t_i.
   Thus, need to be careful with "replanning" where we look at things
   in the past.
*)


type id = int

let zero = 0
let dummy_point = -1
let extension = 10

type edge_constraint = id * id * int * int
    (** (Xi,Xj,a_ij,b_ij): a_ij < Xj - Xi < b_ij *)

type simple_edge = id * int
    (** Xj - Xi < b_ij: edges_in(j), edges_out(i) *)


type t = {
  mutable last : int; (* current number of time points *)
  (* try to change those edges from "list" to "hashtbl" *)
  mutable edges_in : (simple_edge list) array;
  mutable edges_out : (simple_edge list) array;
  (* lower = min_int means available point *)
  mutable lower : int array;
  (* upper = next available point iff lower = min_int *)
  (* upper = max_int means no more available point *)
  mutable upper : int array;
  (* for cycle detection *)
  mutable newly_changed : int array;
  mutable avail_points : int;
  mutable avail_chain_head : int;
}


let set_bit arr bit =
  arr.(bit / 31) <- arr.(bit / 31) lor (1 lsl (bit mod 31))


let reset_bit arr bit =
  arr.(bit / 31) <- arr.(bit / 31) land (lnot (1 lsl (bit mod 31)))


let test_bit arr bit =
  arr.(bit / 31) land (1 lsl (bit mod 31)) <> 0


let create () =
  (* Verb.pe 3 "--- Create STN: base = %f min_int = %d max_int = %d \n"
    ctime min_int max_int; *)
  { last = 0;
    edges_in = Array.make 1 [];
    edges_out = Array.make 1 [];
    lower = Array.make 1 0; (* first time point initialized to [0,0] *)
    upper = Array.make 1 0;
    newly_changed = Array.make 1 0;
    avail_points = 0;
    avail_chain_head = max_int;
  }


let copy stn =
  { stn with
      edges_in = Array.copy stn.edges_in;
      edges_out = Array.copy stn.edges_out;
      lower = Array.copy stn.lower;
      upper = Array.copy stn.upper;
      newly_changed = Array.copy stn.newly_changed;
  }


let add_point stn =
  let next = ref stn.avail_chain_head in
    if !next <> max_int then
      (stn.avail_chain_head <- stn.upper.(!next);
       stn.avail_points <- stn.avail_points - 1)
    else
      (let len = Array.length stn.edges_in in
	 stn.edges_in <- Wrarray.extend stn.edges_in len [];
	 stn.edges_out <- Wrarray.extend stn.edges_out len [];
	 stn.lower <- Wrarray.extend stn.lower len min_int;
	 stn.upper <- Wrarray.extend stn.upper len max_int;
	 assert( stn.avail_points = 0 );
	 for i = len to 2 * len - 2 do
	   stn.upper.(i) <- i + 1
	 done;
	 (* stn.upper.(2 * len - 1) <- max_int; *)
	 if 2 * len > (Array.length stn.newly_changed) * 31 then
	   (let len2 = 2 * len / 31 - len / 31 in
	      stn.newly_changed <- Wrarray.extend stn.newly_changed len2 0);
	 stn.avail_chain_head <- stn.upper.(len);
	 stn.avail_points <- len - 1;
	 next := len);
    if !next > stn.last then
      stn.last <- !next;
    (* stn.lower.(!next) <- min_int; *)
    stn.upper.(!next) <- max_int;
    !next


(***************** adding constraints ****************)


exception Inconsistent


let rec ac3lower stn d (s,dur) =
  (** LB propagation + cycle detection *)
  let lower = stn.lower in
  let update = Math.subtract_ints lower.(d) dur in
    if lower.(s) < update then
      (if stn.upper.(s) < update then
	 raise Inconsistent;
       lower.(s) <- update;
       (* Verb.pe 3 "raising lower bound of %d to %d because lower of %d is %d\n"
	 s lower.(s) d lower.(d); *)
       flush_all ();
       let changed = stn.newly_changed in
	 (* if changed.(s) then *)
	 if test_bit changed s then
	   (* cycle *)
	   raise Inconsistent;
	 (* changed.(s) <- true; *)
	 set_bit changed s;
	 List.iter (ac3lower stn s) stn.edges_in.(s);
	 (* changed.(s) <- false *)
	 reset_bit changed s)


let rec ac3upper stn s (d,dur) =
  (** UB propagation + cycle detection *)
  let upper = stn.upper in
  let update = Math.add_ints upper.(s) dur in
    if upper.(d) > update then
      (if stn.lower.(d) > update then
	 raise Inconsistent;
       upper.(d) <- update;
       (* Verb.pe 3 "lowering upper bound of %d to %d because upper of %d is %d\n"
	 d upper.(d) s upper.(s);
       flush_all (); *)
       let changed = stn.newly_changed in
	 (* if changed.(d) then *)
	 if test_bit changed d then
	   (* cycle *)
	   raise Inconsistent;
	 (* changed.(d) <- true; *)
	 set_bit changed d;
	 List.iter (ac3upper stn d) stn.edges_out.(d);
	 (* stn.newly_changed.(d) <- false *)
         reset_bit changed d)


let add_edge (s,d,dur) stn =
  (** no duplicate (subsumed relation) detection right now.
    MD, Sept 8: dur is in integer already *)
  let s_edge = (s,dur)
  and d_edge = (d,dur) in
    (* Verb.pe 3 "adding edge %d->%d <= %d\n" s d dur;
       flush_all (); *)
    stn.edges_in.(d) <- s_edge::stn.edges_in.(d);
    stn.edges_out.(s) <- d_edge::stn.edges_out.(s);
    ac3lower stn d s_edge;
    ac3upper stn s d_edge


let add_constraint stn (s, d, l, u) =
  if l > min_int then
    add_edge (d,s,(- l)) stn;
  if u < max_int then
    add_edge (s,d,u) stn


let add_constraints stn constraints =
  (* PERHAPS some optimization could be done here to wait on propagation
     until all constraints are added? *)
  List.iter (add_constraint stn) constraints


(***** do not have a constraint removal routine now ********)


(***** making constraints *****)


let fixed_constraint i j d =
  (i, j, d, d)


let lower_constraint i j d =
  (i, j, d, max_int)


let upper_constraint i j d =
  (i, j, min_int, d)


let range_constraint i j l u =
  (i, j, l, u)


let constrain_order stn a b =
  add_constraint stn (lower_constraint a b 0)


(***************** checking relations ******************)


let earliest stn id =
  if id > stn.last then
    failwith (Wrutils.str "invalid stn id %d (max %d)" id stn.last);
  stn.lower.(id)


let latest stn id =
  if id > stn.last then
    failwith (Wrutils.str "invalid stn id %d (max %d)" id stn.last);
  stn.upper.(id)


let print stn =
  for i = 0 to stn.last do
    Verb.pe 3 "%3d: %d - %d\n" i (earliest stn i) (latest stn i)
  done


let stats stn =
  (* stn.last - (Queue.length stn.avail_points), *)
  Array.length stn.edges_in - stn.avail_points,
  (Wrarray.sum_using List.length stn.edges_in)

let print_id id =
  Verb.pe 3 " %d" id

(* Check the add_check_duplicate function in the .bak file to avoid
   adding edges that are subsumed by current edges. May be useful if we
   go for a single time point for each plan and thus there are many edges
   between two given time points *)


(***** cleaning up: remove frozen, past time points  ********)

(* let's be generous by 1 seconds by now *)
let margin = 1


let frozen stn id =
  stn.lower.(id) = stn.upper.(id)


let done_p stn id time =
  (frozen stn id) && (latest stn id < (time - margin))


(* let removed stn id =
  (** assume that if there is some id with no edge_in and no edge_out
    then it's removed *)
  ((List.length stn.edges_in.(id)) = 0) &&
  ((List.length stn.edges_out.(id)) = 0) *)


let removed stn id =
  (* Hashtbl.mem stn.avail_check id *)
  stn.lower.(id) = min_int


let remove_edgein stn des (source,dur) =
  (** remove an edge-in *)
  (* if not ((frozen stn source) or (removed stn des)) then
     add_edge (source,zero,dur - stn.upper.(des)) stn; *)
  (* will not cause any propagation because [des] is frozen *)
  stn.edges_out.(source) <- List.filter (fun (id,dur) -> not (id = des))
    stn.edges_out.(source)


let remove_edgeout stn source (des,dur) =
  (** remove an edge-out *)
  (* if not ((frozen stn des) or (removed stn des) or (des = end_id)) then
     add_edge (zero,des,stn.upper.(source)+dur) stn; *)
  (* will not cause any propagation because [source] is frozen *)
  stn.edges_in.(des) <- List.filter (fun (id,dur) -> not (id = source))
    stn.edges_in.(des)


let cleanup stn ids =
  (** clean up the stn by removing [ids] and constraints related to
    them.  The [ids] are collected by looking at sent plans and analyzing
    resource allocations in the latest context.

    NOTE: some ids may already be removed from the stn due to the following
    reasons: i) resource allocations share points with action's start time.
    Thus, the same time point can be used for two actions, two different
    resources used by the same actions, action's start time and resource
    allocation.

    Be very careful with replanning because we may use those actions again
    and their correspondent time-points are already removed by removing
    the resource allocations.
  *)
  let old_nodes, old_edges = stats stn in
  let remove id =
    (* if (Hashtbl.mem stn.avail_check id) then *)
    if stn.lower.(id) = min_int then
      ()
    else
      (List.iter (fun edge -> remove_edgein stn id edge)
	 stn.edges_in.(id);
       List.iter (fun edge -> remove_edgeout stn id edge)
	 stn.edges_out.(id);
       stn.edges_in.(id) <- [];
       stn.edges_out.(id) <- [];
       stn.lower.(id) <- min_int;
       (* stn.upper.(id) <- max_int *)
       stn.upper.(id) <- stn.avail_chain_head;
       stn.avail_chain_head <- id;
       stn.avail_points <- stn.avail_points + 1)
  in
    List.iter (fun id -> remove id) ids;
    let new_nodes, new_edges = stats stn in
      Verb.pe 3 ";;; Stn.cleanup: Before = (%d,%d) | After = (%d,%d)\n"
	old_nodes old_edges new_nodes new_edges



let cleanup_stn stn time =
  (** find all points in [stn] that : i) frozen; ii) occur before [time]
    and remove them along with their constraints from [stn]. Return
    the ids of removed points. Do not use now but rather rely on
    other functions to collect the timepoints *)
  ()


(*********** testing *************)


let interactive () =
  let stn = create () in
    while true do
      Verb.pe 3 "Add p)oint or c)onstraint (or q)uit): ";
      (match (read_line ()).[0] with
	 'p' -> Verb.pe 3 "made %d\n" (add_point stn)
       | 'c' ->
	   Verb.pe 3 "Enter src, dest, lower, upper: "; flush_all ();
	   Scanf.scanf " %d %d %d %d" (fun s d l u ->
					 Verb.pe 3 "%d - %d within %d - %d\n"
					 s d l u;
					 add_constraints stn [(s, d, l, u)])
       | 'q' -> failwith "user quit"
       | c -> Verb.pe 3 "%c is no good, try again.\n" c);
      print stn
    done


let batch num_pts constraints =
  let stn = create () in
    Wrutils.ntimes (fun () -> ignore (add_point stn)) num_pts;
    List.iter (fun (s, d, l, u) ->
		 print stn;
		 Verb.pe 3 "adding %d -> %d between %d and %d\n%!"
		   s d l u;
		 add_constraints stn
		   [ (s, d, l, u) ])
      constraints;
    print stn


let test1 () =
  batch 5 [ (1, 2, 0, 2);
	    (1, 3, -4, -1);
	    (*(2, 3, -3, 2);
	      (2, 4, 0, 3); *)
	    (3, 2, -2, 3);
	    (4, 2, -3, 0);
	    (3, 5, -2, 1);
	    (4, 5, 1, 3) ]


(*****
lb to    0    1    2    3    4
  0:     0   10   70   50   70
  1:    10   ??   ??   ??   ??
  2:    70   ??   ??   ??   ??  3:    50   ??   ??   ??   ??
  4:    70   ??   ??   ??   ??
******)
let test2 () =
  batch 4 [ (0, 1, 10, 20);
	    (1, 2, 60, max_int);
	    (3, 4, 20, 30);
	    (3, 2, 10, 20);
	    (0, 4, 60, 70) ]


let test3 () =
  let start = Sys.time () in
    try
      batch 3 [ 0, 1, 0, 500;
		0, 2, 1, 600;
		0, 3, 3, 800;
		1, 2, 1, 100;
		2, 3, 2, 200;
		1, 3, 301, 1000 ];
      Verb.pe 3 "SHOULD HAVE BEEN INCONSISTENT!\n%!"
    with Inconsistent ->
      let stop = Sys.time () in
	Verb.pe 3 "Took %f seconds.\n" (stop -. start)


(* EOF *)
