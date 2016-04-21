(** Breadth first search
    Jordan Thayer July 2009 *)


let no_dups sface better_p =
  let i = sface.Search_interface.info in
  let expand_best next n =
    if not(Limit.halt_p i)
    then (Limit.incr_exp i;
	  if sface.Search_interface.goal_p n
	  then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	  else List.iter (fun c -> Dpq.insert next c)
	    (sface.Search_interface.node_expand n))
  in
  let rec expand_layer layer =
    let next = Dpq.create_with better_p sface.Search_interface.initial in
      while (not (Limit.halt_p i)) && (not (Dpq.empty_p layer))
      do
	expand_best next (Dpq.extract_first layer)
      done;
      if (not (Limit.halt_p i)) && (i.Limit.incumbent = Limit.Nothing)
      then expand_layer next in
    expand_layer (Dpq.create_with better_p sface.Search_interface.initial);
    Limit.results5 i


let dups sface better_p =
  let i = sface.Search_interface.info
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
  let expand_best next n =
    if not(Limit.halt_p i)
    then (Limit.incr_exp i;
	  if sface.Search_interface.goal_p n
	  then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	  else List.iter (fun c ->
			    let state = sface.Search_interface.key c in
			      try
				let prev = Htable.find closed state in
				  if not (better_p prev c)
				  then (Htable.replace closed state c;
					Dpq.insert next c)
			      with Not_found ->
				Htable.add closed state c;
				Dpq.insert next c)
	    (sface.Search_interface.node_expand n))
  in
  let rec expand_layer layer =
    let next = Dpq.create_with better_p sface.Search_interface.initial in
      while (not (Limit.halt_p i)) && (not (Dpq.empty_p layer))
      do
	expand_best next (Dpq.extract_first layer)
      done;
      if (not (Limit.halt_p i)) && (i.Limit.incumbent = Limit.Nothing)
      then expand_layer next in
    expand_layer (Dpq.create_with better_p sface.Search_interface.initial);
    Limit.results6 i


let drop_dups sface better_p =
  let i = sface.Search_interface.info
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
  let expand_best next n =
    if not(Limit.halt_p i)
    then (Limit.incr_exp i;
	  if sface.Search_interface.goal_p n
	  then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	  else List.iter (fun c ->
			    let state = sface.Search_interface.key c in
			      if not (Htable.mem closed state)
			      then (Htable.add closed state c;
				    Dpq.insert next c))
	    (sface.Search_interface.node_expand n))
  in
  let rec expand_layer layer =
    let next = Dpq.create_with better_p sface.Search_interface.initial in
      while (not (Limit.halt_p i)) && (not (Dpq.empty_p layer))
      do
	expand_best next (Dpq.extract_first layer)
      done;
      if (not (Limit.halt_p i)) && (i.Limit.incumbent = Limit.Nothing)
      then expand_layer next in
    expand_layer (Dpq.create_with better_p sface.Search_interface.initial);
    Limit.results6 i

(* EOF *)
