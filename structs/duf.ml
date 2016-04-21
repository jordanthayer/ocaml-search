(* $Id: duf.ml,v 1.1 2006/06/30 04:34:59 ruml Exp $

   destructive union find

   each obj is represented by a node in a forest.  Each node is either
   a root or the child of another object's node.  The roots are the
   canonical represetatives of the sets.

   see Sedgewick p446 or CLR
*)


(* if root, store number of nodes in its tree.
   if child, link to obj, not its node, to take advantage of updates. *)
type 'a node = Root of int * 'a | Child of 'a

(* need to find nodes given objects *)
and 'a t = ('a, 'a node) Hashtbl.t


exception Not_new


(***** creation ********)


let make n =
  Hashtbl.create n


let mem uf e =
  Hashtbl.mem uf e


let add uf e =
  if not (mem uf e) then
    Hashtbl.add uf e (Root (1, e))


let add_new uf e =
  if mem uf e then
    raise Not_new
  else
    Hashtbl.add uf e (Root (1, e))


(***** fundamental operations ********)


let rec find_root uf e =
  (** returns a node or raises Not_found *)
  match Hashtbl.find uf e with
    Root _ as n -> n
  | Child parent ->
      match find_root uf parent with
	Root (_,p) as root ->
	  if p != parent then
	    (* path compression *)
	    Hashtbl.replace uf e (Child p);
	  root
      | _ -> failwith "Duf.find_root: root not a Root!"


let join uf a b =
  if a <> b then
    match find_root uf a with
      Root (num_a, root_a) ->
	(match find_root uf b with
	   Root (num_b, root_b) ->
	     if root_a != root_b then
	       (* weight balancing - largest tree remains as root *)
	       if num_a > num_b then
		 let root = Root (num_a + num_b, root_a) in
		   Hashtbl.replace uf root_a root;
		   (* don't need to update a since points directly at root_a *)
		   Hashtbl.replace uf root_b (Child root_a)
	       else
		 let root = Root (num_a + num_b, root_b) in
		   Hashtbl.replace uf root_b root;
		   Hashtbl.replace uf root_a (Child root_b)
	 | _ -> failwith "Duf.join: root of b not a Root")
    | _ -> failwith "Duf.join: root of a not a Root"


let add_join uf a b =
  if not (mem uf a) then
    Hashtbl.add uf a (Child b)
  else
    join uf a b


let same_p uf a b =
  (find_root uf a) == (find_root uf b)


let rep uf a =
  match find_root uf a with
    Root ( _, r) -> r
  | Child _ -> failwith "Duf.rep: root not a Root"


(******** manipulations *********)


let iter f uf =
  Hashtbl.iter (fun k _ -> f k)
    uf


let to_lists uf =
  let classes = Eqvclass.create () in
    iter (fun x -> Eqvclass.add (rep uf x) x classes)
      uf;
    Eqvclass.to_lists classes


let of_list list =
  let uf = make 16 in
    List.iter (add uf) list;
    uf


let of_lists lists =
  let uf = make 16 in
    List.iter (function
		   [] -> ()
		 | first::rest ->
		     add uf first;
		     List.iter (fun x -> add_join uf x first)
		       rest)
      lists;
    uf


(* EOF *)
