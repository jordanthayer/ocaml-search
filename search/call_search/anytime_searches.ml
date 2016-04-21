(** Calls out to anytime searches *)

(** Anytime Weighted A* running on a weight list *)
let anytime_wtlist_astar interface wtlist =
  Anytime_astar_wtlist.no_dups interface wtlist

and anytime_wtlist_astar_dups interface wtlist =
  Anytime_astar_wtlist.dups interface wtlist

and anytime_wtlist_astar_dd interface wtlist =
  Anytime_astar_wtlist.delay_dups interface wtlist

(** Anytime Repairing A*, Likhachev *)
let arastar interface wt_list = Arastar.no_dups interface wt_list
and arastar_dups interface wt_list = Arastar.dups interface wt_list
and arastar_dd interface args = failwith "Arastar delays dups by default."


(** Restarts Weighted A*, heart of LAMA, Richter, Ruml, Thayer *)
let restarts_wastar interface wtlist =
  Restarts_wastar.no_dups interface wtlist

and restarts_wastar_dups interface wtlist =
  Restarts_wastar.dups interface wtlist

and restarts_wastar_dd interface wtlist =
  Restarts_wastar.delay_dups interface wtlist


(** Searches with hard coded weight lists follow *)
let restarts_rl1 interface args =
  restarts_wastar interface (Array.of_list (List.map string_of_float
					      Restarts_wastar.richterl1))

and restarts_rl2 interface args =
  restarts_wastar interface (Array.of_list (List.map string_of_float
					      Restarts_wastar.richterl2))

and restarts_synth interface args =
  restarts_wastar interface (Array.of_list (List.map string_of_float
					      Restarts_wastar.synth_list))

and awastar_rl1 interface args =
  anytime_wtlist_astar interface (Array.of_list (List.map string_of_float
						   Restarts_wastar.richterl1))

and awastar_rl2 interface args =
  anytime_wtlist_astar interface (Array.of_list (List.map string_of_float
						   Restarts_wastar.richterl2))

and awastar_synth interface args =
  anytime_wtlist_astar interface (Array.of_list (List.map string_of_float
						   Restarts_wastar.synth_list))

and awastar_aralist interface args =
  let bound = Search_args.get_float "Anytime_searches.amastar_aralist"
    args 0 in
  anytime_wtlist_astar interface (Array.of_list
				    (List.map string_of_float
				       (Arastar.mk_wtlist bound 0.2)))

and arastar_rl1 interface args =
  arastar interface (Array.of_list (List.map string_of_float
				      Restarts_wastar.richterl1))

and arastar_rl2 interface args =
  arastar interface (Array.of_list (List.map string_of_float
				      Restarts_wastar.richterl2))

and arastar_synth interface args =
  arastar interface (Array.of_list (List.map string_of_float
				      Restarts_wastar.synth_list))

and arastar_default interface args =
  let bound = Search_args.get_float "Anytime_searches.arastar_default"
    args 0 in
  arastar interface (Array.of_list (List.map string_of_float
				      (Arastar.mk_wtlist bound 0.02)))

and restarts_rl1_dups interface args =
  restarts_wastar_dups interface
    (Array.of_list (List.map string_of_float
		      Restarts_wastar.richterl1))

and restarts_rl2_dups interface args =
  restarts_wastar_dups interface (Array.of_list (List.map string_of_float
						   Restarts_wastar.richterl2))

and restarts_synth_dups interface args =
  restarts_wastar_dups interface (Array.of_list (List.map string_of_float
						   Restarts_wastar.synth_list))

and awastar_rl1_dups interface args =
  anytime_wtlist_astar_dups interface (Array.of_list
					 (List.map string_of_float
					    Restarts_wastar.richterl1))

and awastar_rl2_dups interface args =
  anytime_wtlist_astar_dups interface (Array.of_list
					 (List.map string_of_float
					    Restarts_wastar.richterl2))

and awastar_synth_dups interface args =
  anytime_wtlist_astar_dups interface (Array.of_list
					 (List.map string_of_float
					    Restarts_wastar.synth_list))

and awastar_aralist_dups interface args =
  let bound = Search_args.get_float "Anytime_searches.awastar_aralist_dups"
    args 0 in
  anytime_wtlist_astar_dups interface (Array.of_list
					 (List.map string_of_float
					    (Arastar.mk_wtlist bound 0.2)))

and arastar_rl1_dups interface args =
  arastar_dups interface (Array.of_list (List.map string_of_float
					   Restarts_wastar.richterl1))

and arastar_rl2_dups interface args =
  arastar_dups interface (Array.of_list (List.map string_of_float
					   Restarts_wastar.richterl2))

and arastar_synth_dups interface args =
  arastar_dups interface (Array.of_list (List.map string_of_float
					   Restarts_wastar.synth_list))

and arastar_default_dups interface args =
  let bound =
    Search_args.get_float "Anytime_searches.arastar_default_dups" args 0 in
  let delta =
    Search_args.get_float "Anytime_searches.arastar_default_dups" args 1 in
  arastar_dups interface (Array.of_list (List.map string_of_float
					   (Arastar.mk_wtlist bound delta)))

and restarts_rl1_dd interface args =
  restarts_wastar_dd interface (Array.of_list (List.map string_of_float
						 Restarts_wastar.richterl1))

and restarts_rl2_dd interface args =
  restarts_wastar_dd interface (Array.of_list (List.map string_of_float
						 Restarts_wastar.richterl2))

and awastar_rl1_dd interface args =
  anytime_wtlist_astar_dd interface (Array.of_list
				       (List.map string_of_float
					  Restarts_wastar.richterl1))

and awastar_rl2_dd interface args =
  anytime_wtlist_astar_dd interface (Array.of_list
				       (List.map string_of_float
					  Restarts_wastar.richterl2))

and awastar_aralist_dd interface args =
  let bound = Search_args.get_float "Anytime_searches.awastar_aralist_dd"
    args 0 in
  anytime_wtlist_astar_dd interface (Array.of_list
				       (List.map string_of_float
					  (Arastar.mk_wtlist bound 0.2)))

(* EOF *)
