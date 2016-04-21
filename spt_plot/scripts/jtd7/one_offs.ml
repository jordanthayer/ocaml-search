(**

    @author jtd7
    @since 2010-06-28

   Some once off scripts that will be nice to have around
*)


let array_to_point_arrays ar =
  Array.init (Array.length ar) (fun i ->
				  Geometry.point_of_array
				    [| float_of_int i; ar.(i);|])


let get_window_series ?(ykey = "iteration") dset =
  let dset = Dataset.group_by [|"num"|] dset in (* dset is now a list on inst*)
    Array.of_list
      (List.map (fun ds ->
		   (array_to_point_arrays
		      (Dataset.get_column_vector float_of_string
			 [| ykey |] ds).(0)))
	 dset)

let arrays_collection_to_linerr ~names arrays =
  let next_color = Dataset_to_spt.make_color_factory true
  and next_dash = Factories.make_dash_factory [|[||]|] () in
  let next_style = (Dataset_to_spt.make_or_use_line_err_factory
		      next_dash None) in
    List.map2 (fun name l ->
		Num_by_num.line_errbar_dataset (next_style ())
		  ~line_width:(Length.Pt 2.)
		  ?color:(Some (next_color ())) ~name l) names arrays


let grids algs =
  let data = List.map (fun (dsnm,plnm,yk) ->
			 get_window_series ~ykey:yk
			   ((Jtd7_helpers.load_wrap
			       Load_dataset.standard_unit_4_grids)
			      (dsnm,plnm,[]))) algs in
  let dset = arrays_collection_to_linerr
    ~names:(List.map (fun (a,b,c) -> b) algs) data in
  let plot = (Num_by_num.plot ~legend_loc:Legend.Upper_left
		~title:"Grid Problem" ~xlabel:"Iteration"
		~ylabel:"Window Size" dset)
  in plot#display

(* EOF *)
