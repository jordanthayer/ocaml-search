(** Convenient functions to build Spt datasets from RDB datasets.

    @author eaburns
    @since 2010-05-28
*)

open Printf

let df_res = 100

let get_rows ?(res = max_int) ?sort keys ds =
  (** [get_rows keys ds] a shortcut for getting rows of floats. *)
  Verb.pe Verb.debug "getting row keys: ";
  Array.iter (fun s -> Verb.pe Verb.debug "%s " s) keys;
  Verb.pe Verb.debug "\n%!";
  let dt = Dataset.get_row_vector float_of_string ?sort keys ds in
    Verb.pe Verb.debug "%i elements -> " (Array.length dt);
    let dt = Wrarray.lowres res dt in
      Verb.pe Verb.debug "%i elements\n" (Array.length dt);
      dt

(** {1 Factories} ****************************************)

let next_glyph = Factories.default_glyph_factory ()

let next_color_glyph = Factories.default_color_glyph_factory ()

let next_dash = Factories.default_dash_factory ()

let next_line_err = Num_by_num.line_errbar_factory next_dash ()

let next_fill = Factories.default_fill_pattern_factory ()

let make_color_factory ?(default=Drawing.black) use_color =
  (** [make_color_factory ?default use_color] makes a color factory.
      If [use_color] is false then the factory always returns
      black. *)
  if use_color
  then Factories.default_color_factory ()
  else (fun () -> default)


let make_or_use_dash_factory = function
  | None -> Factories.default_dash_factory ()
  | Some f -> f


let make_or_use_glyph_factory color = function
  | None ->
      if color
      then Factories.default_color_glyph_factory ()
      else Factories.default_glyph_factory ()
  | Some f -> f


let make_or_use_line_err_factory next_dash = function
  | None -> Num_by_num.line_errbar_factory next_dash ()
  | Some f -> f


let make_or_use_fill_factory = function
  | None -> Factories.default_fill_pattern_factory ()
  | Some f -> f


(** {1 Scatter plots} ****************************************)

let scatter ~xkey ~ykey ?glyph ?color ?point_radius ds =
  (** [scatter ~xkey ~ykey ?glyph ?color ?point_radius ds] builds a
      scatter plot dataset. *)
  let glyph = match glyph with None -> next_glyph () | Some g -> g in
  let rows = get_rows [| xkey; ykey |] ds in
  let pts = Array.map Geometry.point_of_array rows in
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
    Num_by_num.scatter_dataset glyph ?color ?point_radius ?name pts


let scatters ~xkey ~ykey ?glyph_factory ?(use_color=false)
    ?group_keys
    ?point_radius
    dss =
  (** [scatter ~xkey ~ykey ?glyph_factory ?use_color ?point_radius
      dss] builds a list of scatter plot datasets. *)
  let next_color = make_color_factory use_color in
  let next_glyph = make_or_use_glyph_factory use_color glyph_factory in
    List.map (fun ds ->
		let glyph = next_glyph () and color = next_color () in
		  scatter ~xkey ~ykey ~glyph ~color ?point_radius ds)
      dss


let scatters_key ~xkey ~ykey ~group_key ?glyph_factory ?(use_color=false)
    ?group_keys
    ?point_radius

    dss =
  (** [scatter ~xkey ~ykey ?glyph_factory ?use_color ?point_radius
      dss] builds a list of scatter plot datasets. *)
  let name_fun _ _ v = v in
  let next_color = make_color_factory use_color in
  let next_glyph = make_or_use_glyph_factory use_color glyph_factory in
  let groups = List.map
    (fun ds -> Dataset.group_by ~name_fun [|group_key|] ds) dss in
  let groups = List.flatten groups in

    List.map (fun ds ->
		let glyph = next_glyph () and color = next_color () in
		  scatter ~xkey ~ykey ~glyph ~color ?point_radius ds)
      groups



let bestfit ~xkey ~ykey ?dash ?glyph ?color ?point_radius ds =
  (** [bestfit ~xkey ~ykey ?dash ?glyph ?color ?point_radius ds] builds a
      scatter plot dataset with at bestfit line. *)
  let glyph = match glyph with None -> next_glyph () | Some g -> g in
  let dash = match dash with None -> next_dash () | Some g -> g in
  let rows = get_rows [| xkey; ykey |] ds in
  let pts = Array.map Geometry.point_of_array rows in
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
    Num_by_num.bestfit_dataset ~dashes:dash ~glyph ?color
      ?point_radius ?name pts


let bestfits
    ~xkey ~ykey ?dash_factory ?glyph_factory ?(use_color=false)
    ?point_radius dss =
  (** [bestfits ~xkey ~ykey ?dash_factory ?glyph_factory ?use_color
      ?point_radius dss] builds a list of scatter plot datasets with
      best fit lines. *)
  let next_color = make_color_factory use_color in
  let next_glyph = make_or_use_glyph_factory use_color glyph_factory in
  let next_dash = make_or_use_dash_factory dash_factory in
    List.map (fun ds ->
		let glyph = next_glyph ()
		and dash = next_dash ()
		and color = next_color () in
		  bestfit ~xkey ~ykey ~dash ~glyph ~color ?point_radius ds)
      dss


(** Makes a scatter plot with errorbars.  Each point is the mean x and
    mean y of a group of data based on the group_key.  *)
let scatter_err
    ~xkey ~ykey ~group_keys ?(labeled=true)
    ?xloc ?yloc ?glyph ?color ?point_radius ds =
  let glyph = match glyph with None -> next_glyph () | Some g -> g in
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
  let name_fun _ _ v = v in
  let groups = Dataset.group_by ~name_fun group_keys ds in
  let point_sets =
    Array.of_list
      (List.fold_left (fun set ds ->
			 let rows = get_rows [| xkey; ykey |] ds in
			 let pts = Array.map Geometry.point_of_array rows in
			 let set_name =
			   if labeled
			   then Some (Dataset.name ds)
			   else None
			 in (set_name, pts) :: set)
	 [] groups)
  in
    Num_by_num.scatter_errbar_dataset glyph ?color
      ?point_radius ?xloc ?yloc ?name point_sets


let scatter_errs
    ~xkey ~ykey ~group_keys ?labeled ?glyph_factory
    ?(use_color=false) ?point_radius dss =
  (** [scatter_errs ~xkey ~ykey ~group_keys ?labeled ?glyph_factory
      ?use_color ?point_radius dss] makes a list of scatter plots with
      errorbars. *)
  let next_color = make_color_factory use_color in
  let next_glyph = make_or_use_glyph_factory use_color glyph_factory in
    List.map (fun ds ->
		let glyph = next_glyph () and color = next_color () in
		  scatter_err ~xkey ~ykey ~group_keys ?labeled
		    ~glyph ~color ?point_radius ds)
      dss


let scatter_errs_m2
    ~xkey ~ykey ?labeled ?glyph_factory
    ?(use_color=false) ?point_radius () =
  (** [scatter_errs ~xkey ~ykey ~group_keys ?labeled ?glyph_factory
      ?use_color ?point_radius dss] makes a list of scatter plots with
      errorbars. *)
  let next_color = make_color_factory use_color in
  let next_glyph = make_or_use_glyph_factory use_color glyph_factory in
  (fun (dss,group_keys) ->
    List.map (fun ds ->
		let glyph = next_glyph () and color = next_color () in
		  scatter_err ~xkey ~ykey ~group_keys ?labeled
		    ~glyph ~color ?point_radius ds)
      dss)


let scatter_err_line
    ~xkey ~ykey ~group_key ?(sort=Dataset.Ascending)
    ?(labeled=false) ?glyph ?dash ?color ?line_width ?point_radius ds =
  (** [scatter_err_line ~xkey ~ykey ~group_key ?sort ?labeled ?glyph
      ?dash ?color ?line_width ?point_radius ds] makes a scatter plot
      with errorbars.  Each point is the mean x and mean y of a group
      of data based on the group_key.  *)
  let glyph = match glyph with None -> next_glyph () | Some g -> g in
  let dash = match dash with None -> next_dash () | Some g -> g in
  let name = Dataset.name ds in
  let name_fun _ _ v = v in
  let groups = Dataset.group_by ~sort ~name_fun [| group_key |] ds in
  let point_sets =
    Array.of_list
      (List.fold_left
	 (fun set ds ->
	    let rows = get_rows [| xkey; ykey |] ds in
	    let pts = Array.map Geometry.point_of_array rows in
	    let nm = (Dataset.get_values Fn.identity group_key ds).(0) in
	    let set_name = if labeled then Some nm else None
	    in (pts, set_name) :: set)
	 [] groups)
  in
    Num_by_num.scatter_errbar_lines_dataset
      glyph dash ?color ?point_radius ?line_width ~name point_sets


let scatter_err_lines
    ~xkey ~ykey ~group_key ?sort ?labeled ?glyph_factory ?dash_factory
    ?(use_color=false) ?line_width ?point_radius dss =
  (** [scatter_errs ~xkey ~ykey ~group_key ?labeled ?glyph_factory
      ?dash_factory ?use_color ?line_width ?point_radius dss] makes a
      list of scatter plots with errorbars. *)
  let next_color = make_color_factory use_color in
  let next_glyph = make_or_use_glyph_factory use_color glyph_factory in
  let next_dash = make_or_use_dash_factory dash_factory in
    List.map (fun ds ->
		let glyph = next_glyph ()
		and color = next_color ()
		and dash = next_dash () in
		  scatter_err_line ~xkey ~ykey ~group_key ?labeled
		    ~glyph ~dash ~color ?line_width ?point_radius ds)
      dss

let scatter_err_lines_m2
    ~xkey ~ykey ?sort ?labeled ?glyph_factory ?dash_factory
    ?(use_color=false) ?line_width ?point_radius () =
  (** [scatter_errs ~xkey ~ykey ~group_key ?labeled ?glyph_factory
      ?dash_factory ?use_color ?line_width ?point_radius dss] makes a
      list of scatter plots with errorbars. *)
  let next_color = make_color_factory use_color in
  let next_glyph = make_or_use_glyph_factory use_color glyph_factory in
  let next_dash = make_or_use_dash_factory dash_factory in
  (fun (dss,group_key) ->
    List.map (fun ds ->
      let glyph = next_glyph ()
      and color = next_color ()
      and dash = next_dash () in
      scatter_err_line ~xkey ~ykey ~group_key ?labeled
	~glyph ~dash ~color ?line_width ?point_radius ds)
      dss)



(** {1 Line plots} ****************************************)

let line ~xkey ~ykey ?dash ?color ?line_width ds =
  (** [line ~xkey ~ykey ?dash ?color ?line_width ds] builds a line
      plot dataset. *)
  let dash = match dash with None -> next_dash () | Some g -> g in
  let rows = get_rows ~sort:Dataset.Ascending [| xkey; ykey |] ds in
  let pts = Array.map Geometry.point_of_array rows in
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
    Num_by_num.line_dataset dash ?color ?line_width ?name pts


let lines ~xkey ~ykey ?dash_factory ?(use_color=false) ?line_width dss =
  (** [line ~xkey ~ykey ?dash_factory ?use_color ?line_width dss]
      builds a list of line plot datasets. *)
  let next_color = make_color_factory use_color in
  let next_dash = make_or_use_dash_factory dash_factory in
    List.map (fun ds ->
		let dash = next_dash () and color = next_color () in
		  line ~xkey ~ykey ~dash ~color ?line_width ds)
      dss


let padded_lines
    ~xkey ~ykey ?line_err_factory ?(use_color=false)
    ?line_width dss =
  (** [line_errs ~xkey ~ykey ~group_key ?line_err_factory ?use_color
      ?line_width dss] builds a list of lines with error bars. *)
(*
  let next_color = make_color_factory use_color in
  let next_dash = (if use_color then Factories.make_dash_factory [|[||]|] ()
		   else Factories.default_dash_factory ()) in
*)
    failwith "not implemented"(*
    List.map (fun ds ->
		let dash = next_dash()
		and color = next_color () in
		  pad_lines ~xkey ~ykey ~group_key
		    ~dash ~color ?line_width ds)
      dss*)


let line_point ~xkey ~ykey ?glyph ?dash ?color ?line_width ?point_radius ds =
  (** [line_point ~xkey ~ykey ?glyph ?dash ?color ?line_width
      ?point_radius ds] builds a line plot dataset with glyphs at each
      point. *)
  let dash = match dash with None -> next_dash () | Some g -> g in
  let glyph = match glyph with None -> next_glyph () | Some g -> g in
  let rows = get_rows ~sort:Dataset.Ascending [| xkey; ykey |] ds in
  let pts = Array.map Geometry.point_of_array rows in
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
    Num_by_num.line_points_dataset dash glyph ?color ?line_width
      ?point_radius ?name pts


let line_points
    ~xkey ~ykey ?glyph_factory ?dash_factory ?(use_color=false)
    ?line_width ?point_radius dss =
  (** [line_points ~xkey ~ykey ?glyph_factory ?dash_factory ?use_color
      ?line_width ?point_radius dss] builds a list of line plot
      datasets with glyphs at each point. *)
  let next_color = make_color_factory use_color in
  let next_dash = make_or_use_dash_factory dash_factory in
  let next_glyph = make_or_use_glyph_factory use_color glyph_factory in
    List.map (fun ds ->
		let dash = next_dash ()
		and glyph = next_glyph ()
		and color = next_color () in
		  line_point ~xkey ~ykey ~dash ~glyph ~color
		    ?point_radius ?line_width ds)
      dss


let filter_floats pts =
  (** [filter_floats pts] filters points that have nan or inf for x or
      y. *)
  Wrarray.filter
    (fun p ->
       match classify_float p.Geometry.x, classify_float p.Geometry.y with
	 | FP_nan, _ | _, FP_nan ->
	     Verb.pr Verb.always "Filtering a NAN point\n";
	     false
	 | FP_infinite, _ | _, FP_infinite ->
	     Verb.pr Verb.always "Filtering an inf point %f,%f\n"
	       p.Geometry.x p.Geometry.y;
	     false
	 | _, _ -> true)
    pts


let add_final_point ds pts =
  (** [add_final_point ds pts] adds a point for the final x-value if
      one is specified. *)
  function
    | Some key ->
	let n = Array.length pts in
	let y_final = pts.(n - 1).Geometry.y in
	let x_final_vals = Dataset.get_values float_of_string key ds in
	  if (Array.length x_final_vals) > 1 then
	    failwith "More than 1 final x value";
	  let x_final = x_final_vals.(0) in
	    Wrarray.extend pts 1 (Geometry.point x_final y_final)
    | None -> pts


let line_err ~xkey ?x_final_key ~ykey ~group_keys ?line_err_style
    ?color ?line_width ds =
  (** [line_err ~xkey ?x_final_key ~ykey ~group_keys ?line_err_style
      ?color ?line_width ds] builds a line plot dataset with error
      bars.  The line drawn is the mean of the lines formed by the
      [xkey] and [ykey] on each group.


      If specified, [x_final_key] is the key name to read a 'final x'
      value.  The last actual y-value is coupled with this x-value to
      pad the lines out.  This can be used with, for example, anytime
      data that was all run with a set time-limit but may not have
      returned soltions up to exactly the limit. *)
  let style =
    match line_err_style with None -> next_line_err () | Some g -> g in
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
  let groups = Dataset.group_by group_keys ds in
  let lines =
    Array.of_list
      (List.fold_left
	 (fun set ds ->
	    let rows =
	      get_rows ~res:df_res ~sort:Dataset.Ascending [| xkey; ykey |] ds
	    in
	    let pts = filter_floats (Array.map Geometry.point_of_array rows) in
	      (add_final_point ds pts x_final_key) :: set)
	 [] groups)
  in Num_by_num.line_errbar_dataset style ?line_width ?color ?name lines


let pad_line_err ~xkey ~ykey ~group_keys ?line_err_style ?color ?line_width
    ?minx ?maxx ds =
  (** [line_err ~xkey ~ykey ~group_keys ?line_err_style ?color
      ?line_width ds] builds a line plot dataset with error bars.  The
      line drawn is the mean of the lines formed by the [xkey] and
      [ykey] on each group. *)
  let style =
    match line_err_style with None -> next_line_err () | Some g -> g in
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
  let groups = Dataset.group_by group_keys ds in
  let pad_min = match minx with
    | None -> []
    | Some (x, y) -> [ [| [| x; y |] |] ]
  in
  let pad_max = match maxx with
    | None -> (fun rows -> [])
    | Some x ->
	(fun rows ->
	   let n = Array.length rows in
	   let maxy = try rows.(n - 1).(1) with _ -> 0. in
	     [ [| [| x; maxy |] |] ])
  in

  let step_data lst =
    let rec add prev_y = function
      | [] -> []
      | [|x;y|]::tl -> [|x; prev_y|]::[|x;y|]::(add y tl) in
      (add 0. lst) in

  let lines =
    Array.of_list
      (List.fold_left
	 (fun set ds ->
	    let rows = get_rows ~res:df_res [| xkey; ykey |] ds in
	    let rows' = Array.to_list rows in
	    let rows'' = step_data rows' in
	    let rows = Array.of_list rows'' in
	    let pads = pad_min @ [ rows ] @ (pad_max rows) in
	    let rows = Array.concat pads in
	    let pts = filter_floats (Array.map Geometry.point_of_array rows)
	    in pts :: set) [] groups)
  in Num_by_num.line_errbar_dataset style ?line_width ?color ?name lines



(** [line_errs ~xkey ?x_final_key ~ykey ~group_key ?line_err_factory
    ?use_color ?use_dash ?line_width dss] builds a list of lines
    with error bars. *)
let line_errs
    ~xkey ?x_final_key ~ykey ~group_keys ?line_err_factory ?(use_color=false)
    ?(use_dash=true) ?line_width dss =
  let next_color = make_color_factory use_color in
  let next_dash =
    if not use_dash then
      Factories.make_dash_factory [|[||]|] ()
    else
      Factories.default_dash_factory () in
  let next_style = make_or_use_line_err_factory next_dash line_err_factory in
    List.map (fun ds ->
		let line_err_style = next_style ()
		and color = next_color () in
		  line_err ~xkey ?x_final_key ~ykey ~group_keys
		    ~line_err_style ~color ?line_width ds)
      dss

(** [line_errs ~xkey ~ykey ~group_key ?line_err_factory ?use_color
    ?use_dash ?line_width dss] builds a list of lines with error
    bars. *)
let padded_line_errs ?minx ?maxx ~xkey ~ykey ~group_keys
    ?line_err_factory ?(use_color=false) ?(use_dash=true) ?line_width dss =
  let next_color = make_color_factory use_color in
  let next_dash =
    if not use_dash then
      Factories.make_dash_factory [|[||]|] ()
    else
      Factories.default_dash_factory () in
  let next_style = make_or_use_line_err_factory next_dash line_err_factory in
    List.map (fun ds ->
		let line_err_style = next_style ()
		and color = next_color () in
		  pad_line_err ?minx ?maxx ~xkey ~ykey ~group_keys
		    ~line_err_style ~color ?line_width ds)
      dss


(** {1 Histograms} ****************************************)

let histogram ~key ?dash ?bg_color ?line_width ?bin_width ds =
  (** [histogram ~key ?dash ?bg_color ?line_width ?bin_width
      ds] makes a histogram. *)
  let dash = match dash with None -> next_dash () | Some g -> g in
  let values = Dataset.get_values float_of_string key ds in
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
    Num_by_num.histogram_dataset dash ?line_width
      ?bg_color ?bin_width ?name values


let histograms
    ~key ?dash_factory ?(use_color=false) ?line_width ?bin_width dss =
  (** [histograms ~key ?dash_factory ?use_color ?line_width ?bin_width
      dss] makes a list of histograms. *)
  let next_color = make_color_factory ~default:Drawing.gray use_color in
  let next_dash = make_or_use_dash_factory dash_factory in
    List.map (fun ds ->
		let bg_color = next_color () in
		let dash = next_dash () in
		  histogram ~key ~dash ~bg_color ?line_width ?bin_width ds)
      dss

(** {1 Cumulative density} ****************************************)


let cdf ~key ?normalize ?dash ?color ?line_width ds =
  (** [cdf ~key ?normalize ?dash ?color ?line_width ds] makes a
      cumulative density function. *)
  let dash = match dash with None -> next_dash () | Some g -> g in
  let values = Dataset.get_values float_of_string key ds in
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
    Num_by_num.cdf_dataset dash ?normalize ?line_width ?color ?name values


let cdfs ~key ?normalize ?dash_factory ?(use_color=false) ?line_width dss =
  (** [cdfs ~key ?normalize ?dash_factory ?use_color ?line_width dss]
      makes a list of CDFs. *)
  let next_color = make_color_factory ~default:Drawing.black use_color in
  let next_dash = make_or_use_dash_factory dash_factory in
    List.map (fun ds ->
		let color = next_color () in
		let dash = next_dash () in
		  cdf ~key ?normalize ~dash ~color ?line_width ds)
      dss


(** {1 Box plots} ****************************************)

let boxplot ?outliers ~key ?point_radius ds =
  (** [boxplot ?outliers ~key ?point_radius ds] makes a box plot. *)
  let values = Dataset.get_values float_of_string key ds in
  let name = Dataset.name ds in
    Num_by_nom.boxplot_dataset ?outliers ?point_radius name values


let boxplots ?outliers ~key ?point_radius dss =
  (** [boxplots ?outliers ~key ?point_radius dss] makes a list of
      boxplots. *)
  List.map (fun ds -> boxplot ?outliers ~key ?point_radius ds) dss


let boxplot_groups
    ?outliers
    ~key ~group_key ?(group_name_fun=(fun v -> sprintf "%s %s" v group_key))
    ?(cmp=compare) ?point_radius dss =
  (** [boxplot_groups ?outliers ~key ~group_key ?group_name_fun ?cmp
      ?point_radius dss] builds a set of grouped boxplots.  The plots
      are grouped on [group_key].  Each group is given a name using
      [group_name_fun] which takes a string as an argument where the
      string is the value of the group key for a given group and the
      result is the name of the group.  [cmp] is used to sort the
      groups. *)
  let group_keys = [| group_key |] in
  let next_value = function
    | [] -> failwith "Empty group set"
    | ds :: _ -> Dataset.get_group_value group_key ds
  in
  let min_group_value gsets =
    List.fold_left (fun m gs -> min (next_value gs) m)
      (next_value (List.hd gsets)) (List.tl gsets)
  in
  let next_group gsets =
    let vl = min_group_value gsets in
      Verb.pr Verb.debug "next group value=%s\n" vl;
      let group, gsets' =
	List.fold_left (fun (group, gsets') gs ->
			  let fst, rst = List.hd gs, List.tl gs in
			    if (next_value gs) = vl
			    then begin
			      Verb.pr Verb.debug "adding %s to group\n"
				(Dataset.name fst);
			      fst :: group, rst :: gsets'
			    end else group, gs :: gsets')
	  ([], []) gsets
      in
	((vl, List.rev group),
	 List.rev (List.filter (function [] -> false | _ -> true) gsets'))
  in
  let gsets =
    ref (List.map (Dataset.group_by ~name_fun:(fun n _ _ -> n)
		     ~compare:cmp ~sort:Dataset.Ascending group_keys) dss)
  in
  let groups = ref [] in
    while !gsets <> [] do
      let (vl, dss), gsets' = next_group !gsets in
      let plots = boxplots ?outliers ~key ?point_radius dss in
      let g = Num_by_nom.dataset_group (group_name_fun vl) plots in
	groups := g :: !groups;
	gsets := gsets';
    done;
    List.rev !groups


(** {1 Box plots} ****************************************)


let barchart ~f ?fill ?line_width ds =
  (** [barchart ~f ?fill ?line_width ds] makes a bar chart of the
      value returned by [f] on [ds]. *)
  let fill = match fill with None -> next_fill () | Some g -> g in
  let value = f ds in
  let name = Dataset.name ds in
    Num_by_nom.barchart_dataset fill ?line_width name value


let barcharts ~f ?fill_factory ?line_width dss =
  (** [barchart ~f ?fill_factory ?line_width dss] makes a list of bar
      charts of the value returned by [f] on each dataset in [dss]. *)
  let next_fill = make_or_use_fill_factory fill_factory in
    List.map (fun ds ->
		let fill = next_fill () in
		  barchart ~f ~fill ?line_width ds)
      dss


let barchart_err ~key ?fill ?line_width ds =
  (** [barchart_err ~key ?fill ?line_width ds] makes a bar chart of
      the mean value with error bars. *)
  let fill = match fill with None -> next_fill () | Some g -> g in
  let values = Dataset.get_values float_of_string key ds in
  let name = Dataset.name ds in
    Num_by_nom.barchart_errbar_dataset fill ?line_width name values


let barchart_errs ~key ?fill_factory ?line_width dss =
  (** [barchart_err ~key ?fill_factory ?line_width dss] makes a list
      bar charts of the mean value with error bars. *)
  let next_fill = make_or_use_fill_factory fill_factory in
    List.map (fun ds ->
		let fill = next_fill () in
		  barchart_err ~key ~fill ?line_width ds)
      dss

(** [layered_barchart ~name_key ?name_fun ?sort ~value_key
    ?fill_factory ?line_width ds] builds a layered barchart. *)
let layered_barchart ~name_key ?(name_fun=Fn.identity) ?sort
    ~value_key ?fill_factory ?line_width ds =
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
  let rows =
    Dataset.get_row_vector (fun x -> x) [| name_key; value_key |] ds in
  let values =
    try
      Array.map (fun row -> name_fun row.(0), float_of_string row.(1)) rows
    with x ->
      let str = Printexc.to_string x in
      Printf.eprintf "Failed to convert values from key %s to floats: %s\n"
	value_key str;
      invalid_arg "Dataset_to_spt.layered_barchart: Bad value key" in
  Num_by_nom.layered_barchart_dataset ?fill_factory ?name ?sort ?line_width
    values


(** [layered_barcharts ~name_key ?name_fun ?sort ~value_key
    ?fill_factory_factory ?line_width dss] builds a list of layered
    barcharts. *)
let layered_barcharts
    ~name_key ?name_fun ?sort ~value_key ?fill_factory_factory
    ?line_width dss =
  let fill_factory_factory = match fill_factory_factory with
    | None -> Factories.default_fill_pattern_factory
    | Some ff -> ff
  in
  List.map
    (fun ds ->
      let fill_factory = fill_factory_factory () in
      layered_barchart ~name_key ?name_fun ?sort ~value_key ~fill_factory
	?line_width ds)
    dss


let stacked_barchart ~name_key ~value_key ?fill_factory ?line_width ds =
  (** [stacked_barchart ~name_key ~value_key ?fill_factory ?line_width
      ds] builds a stacked barchart. *)
  let name = if Dataset.name ds <> "" then Some (Dataset.name ds) else None in
  let rows =
    Dataset.get_row_vector (fun x -> x) [| name_key; value_key |] ds in
  let values =
    try
      Array.map (fun row -> row.(0), float_of_string row.(1)) rows
    with x ->
      let str = Printexc.to_string x in
	Printf.eprintf "Failed to convert values from key %s to floats: %s\n"
	  value_key str;
	invalid_arg "Dataset_to_spt.stacked_barchart: Bad value key"
  in
    Num_by_nom.stacked_barchart_dataset ?fill_factory ?name
      ?line_width values


let stacked_barcharts
    ~name_key ~value_key ?fill_factory_factory ?line_width dss =
  (** [layered_barcharts ~name_key ~value_key ?fill_factory_factory
      ?line_width dss] builds a list of stacked barcharts. *)
  let fill_factory_factory = match fill_factory_factory with
    | None -> Factories.default_fill_pattern_factory
    | Some ff -> ff
  in
    List.map (fun ds ->
		let fill_factory = fill_factory_factory () in
		  stacked_barchart ~name_key ~value_key ~fill_factory
		    ?line_width ds)
      dss


(** {1 Heatmaps} ****************************************)

let countmap ~xkey ~ykey ?gradient ?line_width ?bin_size ds =
  (** [countmap ~xkey ~ykey ?gradient ?line_width ?bin_size ds] makes
      a 2-dimensional histogram where. *)
  let rows = get_rows [| xkey; ykey |] ds in
  let pts = Array.map Geometry.point_of_array rows in
    Num_by_num.countmap_dataset ?line_width ?gradient ?bin_size pts


let valuemap ~xkey ~ykey ~zkey ?gradient ?line_width ?bin_size ds =
  (** [valuemap ~xkey ~ykey ~zkey ?gradient ?line_width ?bin_size ds]
      makes a 2-dimensional heatmap where the color is determined by
      [zkey]. *)
  let rows = get_rows [| xkey; ykey; zkey |] ds in
  let trips = Array.map Geometry.triple_of_array rows in
    Num_by_num.valuemap_dataset ?line_width ?gradient ?bin_size trips


(** {1 Nested groups} ****************************************)

type group_descr = {
  key : string;
  (* The group key *)
  name : string -> string;
  (* Function for naming the group *)
  sort : Dataset.sorting;
}


let group_key ?name ?(sort=Dataset.Unsorted) k =
  (** [group_key ?name ?sort k] make a group descriptor for the given
      key. *)
  let name = match name with
    | Some f -> f
    | None -> (fun s -> sprintf "%s=%s" k s)
  in
    { key = k; name = name; sort = sort }


(** [group_datasets singletons sort key dss] groups a list of
    datasets.  If a dataset has no value for the given key then it is
    added directly to all of the groups for this key.  If singletons
    is true then singleton groups are allowed otherwise they are
    filtered out. *)
let group_datasets ~singletons sort key dss =
  let module Strtbl = Hashtbl.Make(struct
    type t = string
    let equal a b = (a:string) = b
    let hash = Hashtbl.hash
  end) in
  let module Strset = Set.Make(String) in
  let name_fun n _ _ = n in
  let tbl = Strtbl.create 149 in
  let vls = ref Strset.empty in
  let no_value = ref [] in
  List.iter (fun ds ->
    if Dataset.is_key key ds then begin
      let grouped = Dataset.group_by [| key |] ~name_fun ds in
      List.iter (fun grp ->
	let k = Dataset.get_group_value key grp in
	vls := Strset.add k !vls;
	Strtbl.add tbl k grp)
	grouped
    end else no_value := ds :: !no_value)
    dss;
  let groups =
    List.filter
      (fun (_, l) -> if not singletons then List.length l > 1 else true)
      (Strset.fold
	 (fun v l -> (v, !no_value @ (List.rev (Strtbl.find_all tbl v))) :: l)
	 !vls [])
  in
  let compare (a, _) (b, _) =
    try
      let a = float_of_string a and b = float_of_string b in
      compare a b
    with _ ->
      try
	let a = int_of_string a and b = int_of_string b in
	compare a b
      with _ ->
	compare a b
  in
  match sort with
    | Dataset.Unsorted -> groups
    | Dataset.Ascending -> List.sort compare groups
    | Dataset.Descending -> List.sort (fun a b -> compare b a) groups
    | Dataset.First -> [List.hd groups]
    | Dataset.Last -> [List.hd (List.rev groups)]


(** [nested_groups group_descrs make_dataset dss] makes a set of
    nested numeric-by-nominal dataset groups.  The inner-most
    datasets are made with the [make_dataset] function which takes a
    Dataset and returns a Num_by_nom_dataset. *)
let nested_groups ?(singletons=true) group_descrs make_dataset dss =
  let rec group_up i dss =
    if i < (Array.length group_descrs) then begin
      let gdesc = group_descrs.(i) in
      let gkey = gdesc.key and gname_fun = gdesc.name and sort = gdesc.sort in
      let grouped = group_datasets ~singletons sort gkey dss in
	List.map (fun (vl, dss') ->
		    let kid = group_up (i + 1) dss' in
		    let gname = gname_fun vl in
		      Num_by_nom.dataset_group gname kid)
	  grouped
    end else
      List.map make_dataset dss
  in
    group_up 0 dss
