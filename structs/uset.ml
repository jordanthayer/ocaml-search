(* $Id: tree.ml,v 1.3 2004/08/17 20:02:02 ruml Exp $

*)


type t = {
  (* index.(i) = absent or location of i in elts *)
  index : int array;
  mutable fp : int;
  (* first fp elements are elements of set. *)
  elts : int array;
}


let absent = -1


let make n =
  { index = Array.make n absent;
    fp = 0;
    elts = Array.make n 0;
  }


let max s =
  (** highest index that can be present in set *)
  Array.length s.index


let check s =
  (** useful for debugging *)
  assert (s.fp >= 0);
  assert (s.fp < (max s));
  Array.iteri (fun i e ->
		 if e <> absent then
		   (assert (e < s.fp);
		    if s.elts.(e) != i then
		      failwith (Wrutils.str
				  "Uset.check: %d is not at %d (%d is)."
				  i e s.elts.(e))))
    s.index;
  for i = 0 to s.fp-1 do
    assert (s.index.(s.elts.(i)) = i)
  done


let count s =
  (** number of items currently present *)
  s.fp


let mem s i =
  s.index.(i) != absent


exception Present


let insert s i =
  (** raises Present  if already present *)
  if mem s i then
    raise Present
  else
    let fp = s.fp in
      s.elts.(fp) <- i;
      s.index.(i) <- fp;
      (* Wrutils.pr "Added %d at %d.\n" i fp; *)
      s.fp <- fp + 1


let remove s i =
  (** raises Not_found if not already present *)
  let x = s.index.(i) in
    if x = absent then
      raise Not_found
    else
      let last = s.fp - 1 in
      let e = s.elts.(last) in
	s.elts.(x) <- e;
	s.index.(e) <- x;
	s.index.(i) <- absent;
	(* Wrutils.pr "Removed %d from %d.\n" i x; *)
	s.fp <- last


exception Empty

let random s =
  if s.fp = 0 then
    raise Empty
  else
    s.elts.(Random.int s.fp)


let iter_unsafe f s =
  for i = 0 to s.fp - 1 do
    f s.elts.(i)
  done


let iter f s =
  (** [f] can safely modify [s] during traverse *)
  Array.iter f (Array.sub s.elts 0 s.fp)


(* EOF *)
