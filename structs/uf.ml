(* union/find data structure for disjoint sets

   see Sedgewick p446 or CLR p ??
*)


module type ORDERED_TYPE =
sig
  type t
  val compare : t -> t -> int
end


module type FDSET =
sig
  type obj
  type t
  val empty : t
  val same_set_p : obj -> obj -> t -> bool
  val join_sets : obj -> obj -> t -> t
end

  
module Make (Ord : ORDERED_TYPE) = struct
  
  type obj = Ord.t
      
  type node = Root of int * obj | Child of obj
    
  module ObjMap = Map.Make(Ord)

  (* interface is functional, but some hidden destructive
     optimizations are done in find_root, so need ref *)
  type t = node ObjMap.t ref


  let empty = ref ObjMap.empty

		
  let rec find_root obj fds =
    (** returns a node or raises Not_found. *)
    match ObjMap.find obj !fds with
      (Root (_, _) as n) -> n
    | Child parent ->
	match find_root parent fds with
	  (Root (_, p) as root) ->
	    if p != parent
	    then
	      (* path compression *)
	      (let n = Child p in
		 fds := ObjMap.add obj n !fds) ;
	    root
	| _ ->  failwith "Uf.find_root: root not a Root"
	    
	    
  let same_set_p a b fds =
    try
      (find_root a fds) == (find_root b fds)
    with Not_found -> false


  let get_root obj fds =
    try find_root obj fds
    with Not_found -> Root (1, obj)
      

  let join_sets a b fds =
    (* find roots. set smaller root to point to larger root. update size *)
    if a = b then
      fds
    else
      match get_root a fds with
	Root (num_a, root_a) ->
	  (match get_root b fds with
	     Root (num_b, root_b) ->
	       if root_a != root_b then
		 (* weight balancing *)
		 if num_a > num_b then
		   let new_root = Root (num_a + num_b, root_a) in
		   let s = ObjMap.add root_a new_root !fds in
		     ref (ObjMap.add root_b (Child root_a) s)
		 else
		   let new_root = Root (num_a + num_b, root_b) in
		   let s = ObjMap.add root_b new_root !fds in
		     ref (ObjMap.add root_a (Child root_b) s)
	       else
		 fds
	   | _ -> failwith "Uf.join_sets: root not a Root")
      | _ -> failwith "Uf.join_sets: root not a Root"


  let representative a fds =
    get_root a fds
      
      
end

  
(* EOF *)
