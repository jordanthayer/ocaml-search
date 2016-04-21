(**

   makes a heat map.

*)


let log2 v = 
  (log10 v) /. (log10 2.)

let do_logs3 xkey ykey zkey dset =
  Dataset.numeric_transform zkey 
    (fun v -> if v = 0. then -5. else log2 v)
    (Dataset.numeric_transform ykey
       (fun v -> if v = 0. then -5. else log2 v)
       (Dataset.numeric_transform xkey
	  (fun v -> if v = 0. then -5. else log2 v) dset))

let do_logs2 xkey ykey dset =
  (Dataset.numeric_transform ykey
     (fun v -> if v = 0. then -5. else log2 v)
     (Dataset.numeric_transform xkey
	(fun v -> if v = 0. then -5. else log2 v) dset))



let make_heat_plot ds x y z title = 
(*
  let ds = do_logs3 x y z ds in
*)
    let cvm = Dataset_to_spt.valuemap 
      ~xkey:x 
      ~ykey:y 
      ~zkey:z
      ~bin_size:{Geometry.x = 1.0;
		 Geometry.y = 1.0;}
      ~line_width:(Length.In 0.50)
      ds in

      let p = Num_by_num.plot 
	~title:title
	~xlabel:("" ^ x)
	~ylabel:("" ^ y)
	~legend_loc:Legend.Upper_right
	[cvm] in
	p#display


(*

let pds = Dataset.load_from_rdb_with_domain 
    ~skip_fails:true
    ~domain:"sliding_tiles" 
    ["alg","wted_astar_dd";
     "model","chris";
    ] 
    ~name:"ha_st_data";;

            
  make_heat_plot pds "wt" "cost" "total nodes expanded" 
"Effect of cost and weight on expansions";;

*)
