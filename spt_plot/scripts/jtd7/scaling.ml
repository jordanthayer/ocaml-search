(**

   @author jtd7
   @since 2010-06-25

   Some nice ways of doing scaling studies on algs
*)

let norm = ref false
let loc = ref Legend.Upper_left
let xmax = ref None
let ymax = ref None

(* A generic caller *)
let scaling_plot ~ykey ~xkey ~group_key ~title ~log10y ~log10x ~use_color
    load algs =
  let dat = load algs in
  let dat = (if !norm
	     then Normalize_dataset.norm_by_alg [|"num";xkey|]
	       ykey (List.hd dat) dat
	     else dat) in
  let dat = (if log10y
	     then List.map (Dataset.numeric_transform ykey log10) dat
	     else dat) in
  let dat = (if log10x
	     then List.map (Dataset.numeric_transform xkey log10) dat
	     else dat) in
  let d = Dataset_to_spt.line_errs ~line_width:(Length.Pt 2.)
    ~use_color ~xkey ~ykey ~group_keys:[|group_key|] dat in
  let xlabel = if log10x then "Log10 "^ xkey else xkey
  and ylabel = if log10y then ("Log10 " ^  ykey) else ykey in
  let ylabel = if !norm then ylabel ^ " relative to A*" else ylabel in
  let plot = (match (!xmax, !ymax) with
		       | None, None ->
			   Num_by_num.plot ~legend_loc:!loc
			     ~title
			     ~xlabel
			     ~ylabel:(if !norm
				      then (ylabel ^ " relative to A*")
				      else ylabel)
			     d
		       | Some x_max, None ->
			   Num_by_num.plot ~legend_loc:!loc
			     ~title
			     ~xlabel
			     ~ylabel:(if !norm
				      then (ylabel ^ " relative to A*")
				      else ylabel)
			     ~x_max
			     d
		       | None, Some y_max ->
			   Num_by_num.plot ~legend_loc:!loc
			     ~title
			     ~xlabel
			     ~ylabel:(if !norm
				      then (ylabel ^ " relative to A*")
				      else ylabel)
			     ~y_max
			     d
		       | Some x_max, Some y_max ->
			   Num_by_num.plot ~legend_loc:!loc
			     ~title
			     ~xlabel
			     ~ylabel:(if !norm
				      then (ylabel ^ " relative to A*")
				      else ylabel)
			     ~x_max
			     ~y_max d) in
    plot#display


(******************************************************************************)

let u4grid_plot ?(ykey = "total nodes generated") ?(log10y = true)
    ?(log10x = true) ?(use_color = true) alg_list =
  let load = List.map (Jtd7_helpers.load_wrap ~o_attrs:["prob", "0.35"]
			 Load_dataset.unit_4_grids) in
    scaling_plot ~ykey ~xkey:"width" ~group_key:"num" ~title:"Grid Four-way 35%"
      ~log10y ~log10x ~use_color load alg_list


let l4grid_plot ?(ykey = "total nodes generated") ?(log10y = true)
    ?(log10x = true) ?(use_color = true) alg_list =
  let load = List.map (Jtd7_helpers.load_wrap ~o_attrs:["prob", "0.35"]
			 Load_dataset.life_4_grids) in
    scaling_plot ~ykey ~xkey:"width" ~group_key:"num" ~title:"Life Grid Four-Way 35%"
      ~log10y ~log10x ~use_color load alg_list


let u8grid_plot ?(ykey = "total nodes generated") ?(log10y = true)
    ?(log10x = true) ?(use_color = true) alg_list =
  let load = List.map (Jtd7_helpers.load_wrap ~o_attrs:["prob", "0.35"]
			 Load_dataset.unit_8_grids) in
    scaling_plot ~ykey ~xkey:"width" ~group_key:"num" ~title:"Grid 4-Way 35%"
      ~log10y ~log10x ~use_color load alg_list


let l8grid_plot ?(ykey = "total nodes generated") ?(log10y = true)
    ?(log10x = true) ?(use_color = true) alg_list =
  let load = List.map (Jtd7_helpers.load_wrap ~o_attrs:["prob", "0.35"]
			 Load_dataset.life_8_grids) in
    scaling_plot ~ykey ~xkey:"width" ~group_key:"num" ~title:"Grid 4-Way 35%"
      ~log10y ~log10x ~use_color load alg_list



let vacuum ?(ykey = "total nodes generated") ?(log10y = true)
    ?(log10x = false) ?(use_color = true) alg_list =

  let load = List.map (Jtd7_helpers.load_wrap Load_dataset.heavy_nodirt) in
    scaling_plot ~ykey ~xkey:"dirt" ~group_key:"num" ~title:"Heavy Vacuum"
      ~log10y ~log10x ~use_color load alg_list


(* EOF *)
