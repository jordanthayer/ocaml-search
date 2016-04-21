(**

   Christopher Wilt

   March 18, 2011

   memory bounded A*.

*)

type 'a node =
{
  data : 'a;
  mutable wf: float;          (* Search order w/ a weighted heuristic value *)
  mutable f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos : int;  (* Position info for dpq *)
  parent: 'a node;
  mutable expanded: bool;
  mutable children: 'a node list;
}

let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let ordered_p a b =
  (** Ordered predicate used for search.  Compares f', then f, then g.
      true if a is better than b.
  *)
  (a.wf < b.wf) ||
  ((a.wf = b.wf) &&
   ((a.f < b.f) ||
    ((a.f = b.f) &&
     (a.g >= b.g))))
let just_f a b =
  (** Sorts nodes solely on total cost information *)
  a.f <= b.f


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand h wt =
  (** Takes the domain [expand] function and a [h]euristic calculator.
      Needs the [wt] which will be applied to the heuristic.
      Creates an expand function which puts the children on the
      parent to be processed as desired. *)
  (fun n ->
     let children =
       List.map (fun (d, g) ->
		   let hv = h d in
		     { data = d;
		       wf = g +. wt *.hv;
		       f = g +. hv;
		       g = g;
		       depth = n.depth + 1;
		       pos = Dpq.no_position;
		       parent = n;
		       expanded = false;
		       children = [];
		     }) (expand n.data n.g) in
       n.children <- children
  )

let mastar sif args =
  let wt = Search_args.get_float "mastar.ml" args 0 in
  let mem_capacity = Search_args.get_int "mastar.ml" args 1 in

  let initial_h = (sif.Search_interface.h
		     sif.Search_interface.initial) in
  let rec initial = {
    data = sif.Search_interface.initial;
    wf = initial_h *. wt;
    f = initial_h;
    g = 0.0;
    depth = 0;
    pos = (-1);
    parent = initial;
    expanded = false;
    children = [];
  } in

  let open_list = Mmh.create ~update_function:setpos ~resize:false
    ordered_p mem_capacity initial in

  let _limit_t = (Limit.make Limit.Nothing sif.Search_interface.halt_on
		   just_f
                   (Limit.make_default_logger
		      (fun n -> n.g)
		      (wrap sif.Search_interface.get_sol_length))) in

  let closed_list = (Htable.create sif.Search_interface.hash
		       sif.Search_interface.equals 10000) in

  let key n = (wrap sif.Search_interface.key) n in
  let ht_add n = Htable.replace closed_list (key n) n in
  let _ht_check n = Htable.mem closed_list (key n) in
  let _ht_remove n = Htable.remove closed_list (key n) in
  let _ht_find n = Htable.find closed_list (key n) in
  let _is_goal n = wrap sif.Search_interface.goal_p n in

    ignore (Mmh.insert open_list initial);
    ht_add initial;

    ()
