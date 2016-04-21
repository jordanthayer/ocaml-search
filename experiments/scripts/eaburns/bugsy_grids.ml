open Grid_runs;;

let scheds =
  [ 30., 0.02;
    10., 0.02;
    3., 0.02; ]

let unit4_utils = [
  0., 1.;
  1., 1e6;
  2., 1e6;
  4., 1e6;
  8., 1e6;
  16., 1e6;
  32., 1e6;
  64., 1e6;
  128., 1e6;
  256., 1e6;
  512., 1e6;
  1., 0.;
];;

let life4_utils = [
  0., 1.;
  100., 1e12;
  200., 1e12;
  400., 1e12;
  800., 1e12;
  16., 1e9;
  32., 1e9;
  64., 1e9;
  1., 0.;
];;

let utils = unit4_utils;;

let grids =
  [ (* Grid.Fourway, Grid.Unit; *)
    Grid.Fourway, Grid.Life; ]

let arastar (way, cost) (init, delta) =
  let args = Printf.sprintf "aras %g %g" init delta in
  let attrs =
    ["alg", "arastar.training";
     "init weight", string_of_float init;
     "delta weight", string_of_float delta ] in
  do_uni_runs ~tipe:"seedinst" "manhattan" false
    ~time_limit:None ~node_limit:None ?exp_limit:None 5000 5000
    attrs args way cost 0.35;;

let arasmon (way, cost) (init, delta) (wf, wt) (dq, dt) =
  let domain = "grid" in
  let root = Filename.concat User_paths.data_root domain in
  let mvstr = match way with Grid.Fourway -> "Four-way" | _ -> "Eight-way" in
  let coststr = match cost with Grid.Unit -> "Unit" | _ -> "Life" in
  let iattrs =
    ["heuristic", "manhattan"; "obstacles", "uniform"; "moves", mvstr;
     "costs", coststr; "prob", "0.35"; "width", "5000"; "height", "5000"; ] in
  let aattrs =
    [ "alg", "arastar.prof"; "init wt", string_of_float init;
      "delta wt", string_of_float delta;
      "qual samples", string_of_int dq;
      "time samples", string_of_int dt; ] in
  let pfile = Rdb.path_for root (iattrs @ aattrs) in
  let args = Printf.sprintf "aras_mon %g %g %g %g %s" init delta wf wt pfile in
  let attrs =
    ["alg", "aras_mon";
     "init weight", string_of_float init;
     "delta weight", string_of_float delta;
     "wf", string_of_float wf;
     "wt", string_of_float wt;
     "qual samples", string_of_int dq;
     "time samples", string_of_int dt; ] in
  do_uni_runs "manhattan" false
    ~time_limit:None ~node_limit:None ?exp_limit:None 5000 5000
    attrs args way cost 0.35;;

let dtaserr dlim (way, cost) =
  let domain = "grid" in
  let root = Filename.concat User_paths.data_root domain in
  let mvstr = match way with Grid.Fourway -> "Four-way" | _ -> "Eight-way" in
  let coststr = match cost with Grid.Unit -> "Unit" | _ -> "Life" in
  let aattrs =
    ["alg", "dtaserr"; "depth horizon", string_of_int dlim; ] in
  let iattrs =
    ["heuristic", "manhattan"; "obstacles", "uniform"; "moves", mvstr;
     "costs", coststr; "prob", "0.35"; "width", "5000"; "height", "5000";] in
  let errfile = Rdb.path_for root (iattrs @ aattrs @ ["num", "errs"]) in
  let args = Printf.sprintf "dtaserr %d %s" dlim errfile in
  do_uni_runs ~tipe:"training" "manhattan" false
    ~time_limit:None ~node_limit:None ?exp_limit:None 5000 5000
    aattrs args way cost 0.35;;


let bugsy correcth dropdups estdelay (way, cost) (wf, wt) =
  let args = if correcth then "true" else "false" in
  let args = if dropdups then args ^ " true" else args ^ " false" in
  let args = if estdelay then args ^ " true" else args ^ " false" in
  let args = Printf.sprintf "bugsy %g %g %s" wf wt args in
  let attrs =
    ["alg", "bugsy";
     "wf", string_of_float wf;
     "wt", string_of_float wt;
     "correct h", string_of_bool correcth;
     "drop dups", string_of_bool dropdups;
     "estimate delay", string_of_bool estdelay; ] in
  do_uni_runs "manhattan" false ~time_limit:None ~node_limit:None
    ?exp_limit:None 5000 5000 attrs args way cost 0.35;;

let alg (way, cost) args attrs =
  do_uni_runs "manhattan" false ~time_limit:None ~node_limit:None
    ?exp_limit:None 5000 5000 attrs args way cost 0.35;;

List.iter
  (fun prob ->
    List.iter
      (fun sch ->
	List.iter
	  (fun u ->
	    arasmon prob sch u (100, 100);
	    arasmon prob sch u (500, 500);
	    arasmon prob sch u (1000, 1000);)
	  utils)
      scheds)
  grids;;

List.iter
  (fun prob ->
    List.iter
      (fun u ->
	bugsy true true true prob u;
	bugsy true true false prob u;
	bugsy true false true prob u;
	bugsy true false false prob u;
	bugsy false true true prob u;
	bugsy false true false prob u;
	bugsy false false true prob u;
	bugsy false false false prob u;)
      utils)
  grids;;

List.iter (fun prob -> List.iter (arastar prob) scheds) grids;;

List.iter (fun prob -> List.iter (fun dlim -> dtaserr dlim prob) [10]) grids;;

List.iter (fun prob -> alg prob "speedy" ["alg", "speedy"]) grids;;

let bugsy_old (way, cost) (wf, wt) =
  let args = Printf.sprintf "bugsy-old %g %g 130000" wf wt in
  let attrs =
    ["alg", "bugsy-old";
     "wf", string_of_float wf;
     "wt", string_of_float wt; ] in
  do_uni_runs "manhattan" false ~time_limit:None ~node_limit:None
    ?exp_limit:None 5000 5000 attrs args way cost 0.35;;

List.iter (fun prob -> List.iter (bugsy_old prob) utils) grids;;

let wastar subtree (way, cost) wt =
  let args = Printf.sprintf "wastar %g %s" wt (string_of_bool subtree) in
  let attrs =
    ["alg", "wastar";
     "wt", string_of_float wt;
     "update subtree", string_of_bool subtree ] in
  do_uni_runs "manhattan" false ~time_limit:None ~node_limit:None
    ?exp_limit:None 5000 5000 attrs args way cost 0.35;;

List.iter
  (fun wt ->
    wastar false (Grid.Fourway, Grid.Unit) wt;
    wastar true (Grid.Fourway, Grid.Unit) wt;
    wastar false (Grid.Fourway, Grid.Life) wt;
    wastar true (Grid.Fourway, Grid.Life) wt;)
  [ 1.01; 1.1; 1.2; 1.4; 2.0; 4.0; 8.0 ]

