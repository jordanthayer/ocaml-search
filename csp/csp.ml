(* $Id: csp.ml,v 1.1 2004/01/13 23:45:32 ruml Exp ruml $

   data structures for CSPs
*)
		   

(* var, val *)
type nogood = (int * int) list


(* I try to be careful to avoid allowing direct access to p.domains to
   ensure that a problem instance is read-only.  [reduce_domains] gives
   access to the fields, but returns fresh copies *)
type problem = {
  domains : (int list) array;
  nogoods : nogood list;
}


let num_vars p =
  Array.length p.domains


let max_domain_size p =
  Wrarray.max_by List.length p.domains
    

(*************** pre- and post-processing ***************)

    
let unassigned = -999999


let reduce_domains p =
  (** returns fresh copies of the domains and nogoods of [p] without
    duplicates.  processes and removes unary constraints.  doesn't try
    to determine sat/unsast: doesn't do arc-consistency, doesn't do any
    assigning of values to variables. *)
  let d = Array.map Wrlist.remove_duplicates p.domains in
    Array.iter (fun l -> assert (not (List.mem unassigned l)))
      d;
    let ngs = Wrlist.remove_duplicates p.nogoods in
      (*** remove subsumed nogoods: another nogood is subset of it ****)
    let ngs = List.filter (function
			       (var, value)::[] ->
				 d.(var) <- Wrlist.remove_first value d.(var);
				 false
			     | _ -> true)
		ngs
      (*** remove irrelevant nogoods: values not in domain ***)
    in
      d, ngs


let violates a ngs =
  List.exists (fun ng ->
		 List.for_all (fun (var, value) ->
				 a.(var) == value)
		 ng)
    ngs
    

let satisfies a p =
  (** returns true iff assignment [a] satisfies problem [p] - all vars
    given legal values and no nogoods violated *)
  (Wrarray.for_alli (fun var value ->
		       List.mem value p.domains.(var))
     a) &&
  (not (violates a p.nogoods))


let check sol csp sat =
  let truth = satisfies sol csp in
    assert (sat = truth)


(* EOF *)
