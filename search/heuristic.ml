(**

    @author jtd7
    @since 2011-02-14
*)

type properties =
  | Admissible
  | Inadmissible
  | Consistent
  | Inconsistent
  | Cost
  | Distance
  | Cheapest
  | Nearest
  | Forwards
  | Backwards
  | Memory

type 'state hd = ('state -> float * float)
type 'state fixed = ('state -> float)
type 'state between = ('state -> 'state -> float)
type 'state between_hd = ('state -> 'state -> float * float)

type 'heuristic_type heuristic = {
  name : string;
  properties : properties list;
  heuristic : 'heuristic_type
}

type 'state heuristics = {
  hd : ('state hd) heuristic list;
  fixed : ('state fixed) heuristic list;
  between : ('state between) heuristic list;
  between_hd : ('state between_hd) heuristic list;
}

let empty_heuristics =
  { hd = [];
    fixed = [];
    between = [];
    between_hd = []; }

(******** Fuctions Follow *********)

let print_heuristic ?(verb = Verb.toplvl) h =
  Verb.pe verb "Heuristic: %s\n%!" h.name


let with_properties heuristics properties =
  (** Given a list of heuristic,
      returns those elements that match all of the properties *)
  List.filter (fun h ->
		 List.fold_left
		   (fun accum e -> accum && (List.mem e h.properties))
		   true properties) heuristics


let without_properties heuristics properties =
  (** Given a list of heuristic,
      returns those elements that match none of the properties *)
  List.filter (fun h ->
		 List.fold_left
		   (fun accum e -> accum && not (List.mem e h.properties))
		   true properties) heuristics


let by_name heuristics name =
  (* you should change this to regexp contains instead of = at some point *)
  { hd = List.filter (fun h -> h.name = name) heuristics.hd;
    fixed = List.filter (fun h -> h.name = name) heuristics.fixed;
    between = List.filter (fun h -> h.name = name) heuristics.between;
    between_hd = List.filter (fun h -> h.name = name) heuristics.between_hd }


let name_first heuristics name =
  (* sorts the desired name to the fore, should use regexps instead of
     equality eventually *)
  let sfun name1 name2 = (if name1.name = name then 1
			  else if name2.name = name then -1
			  else 0) in
  { hd = List.sort sfun heuristics.hd;
    fixed = List.sort sfun heuristics.fixed;
    between = List.sort sfun heuristics.between;
    between_hd = List.sort sfun heuristics.between_hd; }


let get_hd heuristics properties =
  let res = with_properties heuristics.hd properties in
    List.iter print_heuristic res;
    res


let get_fixed heuristics properties =
  let res = with_properties heuristics.fixed properties in
    List.iter print_heuristic res;
    res


let get_between heuristics properties =
  let res = with_properties heuristics.between properties in
    List.iter print_heuristic res;
    res


let get_between_hd heuristics properties =
  let res = with_properties heuristics.between_hd properties in
    List.iter print_heuristic res;
    res


let default_hd heuristics =
  let res = List.hd (with_properties heuristics.hd [Forwards; Admissible]) in
    print_heuristic res;
    res


let default_fixed heuristics =
  let res = List.hd (with_properties heuristics.fixed
		       [Cost; Forwards; Admissible]) in
    print_heuristic res;
    res


let default_between heuristics =
  let res = List.hd (with_properties heuristics.between [Admissible]) in
    print_heuristic res;
    res


let default_between_hd heuristics =
  let res = List.hd (with_properties heuristics.between_hd [Admissible]) in
    print_heuristic res;
    res


(* EOF *)
