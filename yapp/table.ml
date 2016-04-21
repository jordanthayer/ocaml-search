(* $Id: results.ml,v 1.1 2005/01/30 19:05:42 ruml Exp ruml $

   analyzing results
*)


let ordinal_sets data_sets data_label precision =
  (** data_sets is a ((((float array), string) array), string) list.  each
    set is an array of (data, name) pairs, one for each ordinal place on the
    x axis, paired with a string naming the condition.

    table has sets along the top, "ordinal places" downward.  *)
  (* print col labels *)
  Wrutils.pr "\\begin{tabular}{r|%s}\n"
    (String.make (List.length data_sets) 'r');
  Wrutils.pr "%s " data_label;
  List.iter (fun (_, name) ->
	       Wrutils.pr "&\t%s " name)
    data_sets;
  Wrutils.pr "\\\\ \\hline \n";
  match data_sets with
    [] -> failwith "no data sets for table"
  | first_set::_ ->
      let num_rows = Array.length (fst first_set) in
	for i = 0 to num_rows - 1 do
	  let row_name = snd (fst first_set).(i) in
	    Wrutils.pr "%s " row_name;
	    List.iter (fun (data,_) ->
			 let vals,name = data.(i) in
			   if name <> row_name then
			     failwith "inconsistent row names in data";
			   let mean = Stats.mean vals in
			     Wrutils.pr "&\t%.*f " precision mean)
	      data_sets;
	    Wrutils.pr "\\\\\n"
	done;
	Wrutils.pr "\\end{tabular}\n"


let ordinal_sets_e data_sets data_label precision =
  (** data_sets is a ((((float array), string) array), string) list.  each
    set is an array of (data, name) pairs, one for each ordinal place on the
    x axis, paired with a string naming the condition.

    table has sets along the top, "ordinal places" downward.  *)
  (* print col labels *)
  Wrutils.pr "\\begin{tabular}{r|%s}\n"
    (String.make (List.length data_sets) 'r');
  Wrutils.pr "%s " data_label;
  List.iter (fun (_, name) ->
	       Wrutils.pr "&\t%s " name)
    data_sets;
  Wrutils.pr "\\\\ \\hline \n";
  match data_sets with
    [] -> failwith "no data sets for table"
  | first_set::_ ->
      let num_rows = Array.length (fst first_set) in
	for i = 0 to num_rows - 1 do
	  let row_name = snd (fst first_set).(i) in
	    Wrutils.pr "%s " row_name;
	    List.iter (fun (data,_) ->
			 let vals,name = data.(i) in
			   if name <> row_name then
			     failwith "inconsistent row names in data";
			   let mean = Stats.mean vals in
			     Wrutils.pr "&\t%.*e " precision mean)
	      data_sets;
	    Wrutils.pr "\\\\\n"
	done;
	Wrutils.pr "\\end{tabular}\n"


(* EOF *)
