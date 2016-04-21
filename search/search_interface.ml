(**
   An interface which provides access to all of the tools a search could
   possible need.  Used to make the footprints of searches absolutely
   identical
*)

type domain =
  | UGrid
  | LGrid
  | Tiles
  | Inv_tiles
  | Sequence
  | Salesman
  | TPlan
  | OPlan
  | Robot
  | Anticipation
  | Vacuum
  | Heavy_vacuum
  | Vacuum_maze
  | Synthetic
  | Rucksack
  | Pancake
  | Logistics
  | Sokoban
  | Vis_nav
  | Dock_robot
  | Openstacks
  | Hanoi
  | Solitaire_chess
  | Topspin
  | Scanalyzer
  | Manufacture
  | Santa
  | Robot_plan
  | Orc

type ('node, 'key, 'cost) interface = {
  (** The interface to searches.  Contains all of the tools a search might
      need in order to operate
      'node is the type of node (the domain)
      'key is the type of the key function used by the domain
  *)

  (* Heuristics *)
  h : 'node -> 'cost;
  d : 'node -> 'cost;
  t : 'node -> int;

  (* a node type. *)

  hd: 'node -> 'cost * 'cost; (* Hd's are provided because sometimes *)
  rev_h: 'node -> 'cost;      (* Calculating both at once is cheaper *)
  rev_d: 'node -> 'cost;
  rev_hd: 'node -> 'cost * 'cost;

  cost_between : 'node -> 'node -> 'cost;
  (* Estimate the cost between two nodes. *)

  dist_between : 'node -> 'node -> 'cost;
  (* Estimate the distance between two nodes. *)

  domain_expand: 'node -> 'cost -> ('node * 'cost) list;
  predecessor : 'node -> 'cost -> ('node * 'cost) list;
  node_expand: 'node -> 'node list;
  resort_expand: 'node -> bool * ('node list);
  key: 'node -> 'key;
  key_printer: 'key -> string;
  hash: 'key -> int;
  goal_p: 'node -> bool;
  get_sol_length: 'node -> int;
  incumbent: 'node Limit.return;
  halt_on : Limit.t list;
  initial: 'node;
  info : 'node Limit.info;
  equals : 'key -> 'key -> bool;
  domain : domain;
  parent_update : 'node -> 'node -> unit;

  heuristics : 'node Heuristic.heuristics;

  (* Functions for frontier search *)

  new_subproblem :
    'node Limit.info -> init:'node -> target:'node ->
					  ('node, 'key, 'cost) interface;
  (** Makes a new sub-search from [init] to [target]. *)

  get_neighbor_operators : 'node -> ('key * int) list;
  (** Gets the neighbors of the current node along with the operator
      which, if applied to the neighbor will re-generate the current
      node. *)

  get_generating_operator : 'node -> int;
  (** Get the operator that was just applied to the parent to generate
      the current node. *)
}


let unspeced_0_arg str =
  (** An unspecified 0 argument function which tells you that the
      particular function was not specified during the search
      interface creation *)
    (fun () -> failwith (str ^ " was not supplied to search interface!."))

and unspeced_1_arg str =
  (** An unspecified one argument function which tells you that the particular
      function was not specified during the search interface creation *)
  (fun _ -> failwith (str ^ " was not supplied to search interface!."))


and unspeced_2_arg str =
  (** An unspecified two argument function which tells you that the particular
      function was not specified during the search interface creation *)
      (fun _ _ -> failwith (str ^ " was not supplied to search interface!."))


let make ?(h = unspeced_1_arg "h") ?(d = unspeced_1_arg "d")
    ?(t = unspeced_1_arg "t")
    ?(hd = unspeced_1_arg "hd") ?(rev_h = unspeced_1_arg "rev_h")
    ?(rev_d = unspeced_1_arg "rev_d") ?(rev_hd = unspeced_1_arg "rev_hd")
    ?(cost_between = unspeced_2_arg "cost_between")
    ?(dist_between = unspeced_2_arg "dist_between")
    ?(domain_expand = unspeced_2_arg "domain_expand")
    ?(predecessor = unspeced_2_arg "predecessor")
    ?(node_expand = unspeced_1_arg "node_expand")
    ?(resort_expand = unspeced_1_arg "resort_expand")
    ?(key = unspeced_1_arg "key") ?(key_print = unspeced_1_arg "key_printer")
    ?(hash = Hashtbl.hash)
    ?(goal_p = unspeced_1_arg "goal_p") ?(incumbent = Limit.Nothing)
    ?(halt_on = [Limit.Never]) ?(get_sol_length = (fun _ -> -1))
    ?(equals = (=))
    ?(wt_vect = [||]) ?(p_update = unspeced_2_arg "parent update")
    ?(new_subproblem =
	(fun _ ~init:_ ~target:_ ->
	   failwith ("new_subproblem not supplied to search interface!")))
    ?(get_neighbor_operators=unspeced_1_arg "get_neighbor_operators")
    ?(get_generating_operator=unspeced_1_arg "get_generating_operator")
    ?(heuristics = Heuristic.empty_heuristics)
    ?info
    domain initial better_p log =
  (** Creates a search interface from the supplied parameters.
      [h]
      [d]
      [t]
      [hd]
      [rev_h]
      [rev_d]
      [rev_hd]
      [cost_between]
      [dist_between]
      [domain_expand]
      [node_expand]
      [resort_expand]
      [key]
      [goal_p]
      [incubent]
      [halt_on]
  *)
  { h = h;
    d = d;
    t = t;
    hd = hd;
    rev_h = rev_h;
    rev_d = rev_d;
    rev_hd = rev_hd;
    cost_between = cost_between;
    dist_between = dist_between;
    domain_expand = domain_expand;
    predecessor = predecessor;
    node_expand = node_expand;
    resort_expand = resort_expand;
    key = key;
    key_printer = key_print;
    hash = hash;
    goal_p = goal_p;
    get_sol_length = get_sol_length;
    incumbent = incumbent;
    initial = initial;
    halt_on = halt_on;
    heuristics = heuristics;
    info = (match info with
	      | None -> Limit.make incumbent halt_on better_p log
	      | Some i -> i);
    equals = equals;
    domain = domain;
    parent_update = p_update;
    new_subproblem = new_subproblem;
    get_neighbor_operators = get_neighbor_operators;
    get_generating_operator = get_generating_operator;
  }


let alter ?(h = None) ?(d = None) ?(hd = None) ?(rev_h = None)
    ?(rev_d = None) ?(rev_hd = None)
    ?(cost_between = None)
    ?(dist_between = None)
    ?(domain_expand = None)
    ?(predecessor = None)
    ?(node_expand = None) ?(resort_expand = None)
    ?(key = None) ?(key_print = None) ?(hash = None)
    ?(goal_p = None) ?(incumbent = None) ?(halt_on = None) ?(equals = None)
    ?(wt_vect = None) ?(p_update = None) ?(initial = None) ?(info = None)
    ?(heuristics = None)
    ?new_subproblem
    ?get_neighbor_operators
    ?get_generating_operator
    sface =

  { h = (match h with
	   | None -> sface.h
	   | Some fn -> fn);
    d = (match d with
	   | None -> sface.d
	   | Some fn -> fn);
    t = sface.t;
    hd = (match hd with
	    | None -> sface.hd
	    | Some fn -> fn);
    rev_h = (match rev_h with
	       | None -> sface.rev_h
	       | Some fn -> fn);
    rev_d = (match rev_d with
	       | None -> sface.rev_d
	       | Some fn -> fn);
    rev_hd = (match rev_hd with
		| None -> sface.rev_hd
		| Some fn -> fn);
    cost_between = (match cost_between with
		      | None -> sface.cost_between
		      | Some f -> f);
    dist_between = (match dist_between with
		      | None -> sface.dist_between
		      | Some f -> f);
    domain_expand = (match domain_expand with
		       | None -> sface.domain_expand
		       | Some fn -> fn);
    predecessor = (match predecessor with
		     | None -> sface.predecessor
		     | Some fn -> fn);
    node_expand = (match node_expand with
		     | None -> sface.node_expand
		     | Some fn -> fn);
    resort_expand = (match resort_expand with
		       | None -> sface.resort_expand
		       | Some fn -> fn);
    key = (match key with
	     | None -> sface.key
	     | Some fn -> fn);
    key_printer = (match key_print with
		     | None -> sface.key_printer
		     | Some fn -> fn);
    hash = (match hash with
	      | None -> sface.hash
	      | Some fn -> fn);
    goal_p = (match goal_p with
		| None -> sface.goal_p
		| Some fn -> fn);

    get_sol_length = sface.get_sol_length;
    incumbent = (match incumbent with
		   | None -> sface.incumbent
		   | Some i -> i);
    halt_on = (match halt_on with
		 | None -> sface.halt_on
		 | Some ls -> ls);
    initial = (match initial with
		 | None -> sface.initial
		 | Some i -> i);
    heuristics = (match heuristics with
		    | None -> sface.heuristics
		    | Some h -> h);
    info = (match info with
		None -> sface.info
	      | Some i -> i);
    equals = (match equals with
		| None -> sface.equals
		| Some fn -> fn);
    domain = sface.domain; (* we don't permit changes here *)
    parent_update = (match p_update with
		       | None -> sface.parent_update
		       | Some fn -> fn);
    new_subproblem = (match new_subproblem with
			| None -> sface.new_subproblem
			| Some v -> v);
    get_neighbor_operators = (match get_neighbor_operators with
				| None -> sface.get_neighbor_operators
				| Some v -> v);
    get_generating_operator = (match get_generating_operator with
				 | None -> sface.get_generating_operator
				 | Some v -> v);
  }


let get_default_fixed sface =
  (** Useful until all domains implement the heuristic function thingy *)
  let heuristics = sface.heuristics in
    try (Heuristic.default_fixed heuristics).Heuristic.heuristic
    with _ -> sface.h


let get_default_hd sface =
  let heuristics = sface.heuristics in
    (** Useful until all domains implement the heuristic function thingy *)
    try (Heuristic.default_hd heuristics).Heuristic.heuristic
    with _ -> sface.hd

(* EOF *)
