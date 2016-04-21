(* $Id: csp.ml,v 1.1 2004/01/13 23:45:32 ruml Exp ruml $

   data structures for CSPs
*)
		   

(* var, val *)
type nogood = (int * int) list

    
type problem = {
  domains : (int list) array;
  nogoods : nogood list;
}
    

(*************** basic utilities ***************)


let num_vars p =
  Array.length p.domains


let max_domain_size p =
  Wrarray.max_by List.length p.domains


let domains p =
  (** returns a copy to prevent anyone from modifying the problem, so
    same input can be reused *)
  Array.copy p.domains

let nogoods p =
  p.nogoods


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
