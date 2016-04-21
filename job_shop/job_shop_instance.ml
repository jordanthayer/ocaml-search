(** Job shop scheduling instances.

    @author eaburns
    @since 2010-02-16
*)


type t = {
  n : int;
  m : int;
  jobs : job array;

  ids : (int * int) array;
  (* ids -> (job, op) mapping is cached because profiling showed that
     it was more expensive to compute than was worth in. *)
}

and job = operation array

and operation = {
  mutable machine : int;
  duration : int;
}

let no_duration = ~-1

let null_operation = { machine = min_int; duration = no_duration }
  (** [null_operation] a placeholder operation. *)


let make_span_ub t =
  (** [make_span_ub t] get an upper bound on the optimal make span. *)
  let sum = ref 0 in
    for i = 0 to t.n - 1 do
      let ops = t.jobs.(i) in
	for j = 0 to t.m - 1 do
	  sum := !sum + ops.(j).duration
	done
    done;
    !sum


(** {6 Operations} ********************)

let number_of_ops t = t.n * t.m
  (** [number_of_ops t] gets the number of operations in the
      instance. *)


let id_of_op t job op = ((job * t.m) + op) + 1
  (** [id_of_op t job op] gets a unique, non-zero integer id for the
      specific operation. *)


let op_of_id t id =
  (** [op_of_id t id] gets the (job, op) tuple given an id. *)
  try
    t.ids.(id - 1)
  with _ -> begin
    Printf.printf "id=%d\n" id;
    assert false
  end


let conflicting_op_ids t =
  (** [conflicting_op_ids t] gets a list pairs of operation ids for
      operations that conflict for the same resource. *)
  let bins = Hashtbl.create 100 in
    for i = 0 to t.n - 1 do
      let ops = t.jobs.(i) in
	for j = 0 to t.m - 1 do
	  let op = ops.(j) in
	    Hashtbl.add bins op.machine (id_of_op t i j)
	done
    done;
    Wrlist.mapcan (fun m ->
		     let jobs = Hashtbl.find_all bins (m - 1) in
		       Wrlist.pairs jobs)
      (Wrlist.range t.m)


let duration t id =
  (** [duration t id] gets the duration of the job specified by the
      given id. *)
  let n, m = op_of_id t id in
    t.jobs.(n).(m).duration


(** {6 Jobs} ********************)

let job_op_ids t =
  (** [job_op_ids t] gets a list of operation ID lists for each
      job. *)
  Array.to_list
    (Array.mapi (fun job ops ->
		   Array.to_list (Array.mapi (fun op _ ->
						id_of_op t job op) ops)
		)
       t.jobs)


(** {6 Creating Instances} ********************)

let make n m jobs =
  let ids = Array.create (n * m) (~-1, ~-1) in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
	try
	  ids.(i * m + j) <- i, j
	with _ ->
	  (Printf.printf "n=%d m=%d n * m = %d, i = %d, j = %d, ind = %d\n%!"
	     n m (n * m) i j (i * n + j);
	   assert false);
      done
    done;
    {
      n = n;
      m = m;
      jobs = jobs;
      ids = ids;
    }


(** {6 I/O} ********************)

let instance_root = User_paths.instance_root ^ "job_shop/"

let write t file =
  Verb.pr Verb.often "Writing to %s\n" file;
  Wrio.with_outfile file (fun outch ->
			    Printf.fprintf outch "%d %d\n" t.n t.m;
			    for i = 0 to t.n - 1 do
			      let ops = t.jobs.(i) in
				for j = 0 to t.m - 1 do
				  let op = ops.(j) in
				    Printf.fprintf outch "%d %d "
				      op.machine op.duration
				done;
				Printf.fprintf outch "\n";
			    done)

let read inch =
  (** [read file] reads an instance from the given channel. *)
  let n = Wrio.input_int inch and m = Wrio.input_int inch in
  let t = make n m (Array.make_matrix n m null_operation) in
    for i = 0 to n - 1 do
      let ops = t.jobs.(i) in
	for j = 0 to m - 1 do
	  let mach = Wrio.input_int inch
	  and dur = Wrio.input_int inch
	  in ops.(j) <- { machine = mach; duration = dur }
	done
    done;
    t


let load path =
  let inch = open_in path in
  let t = read inch in
    close_in inch;
    t


let save ?(overwrite=false) t model keys =
  (** [save ?overwrite t model keys] saves an instance [t] to the
      appropriate RDB for [model]. *)
  let path =
    Rdb.path_for
      instance_root
      ([ "model", model;
	 "num_jobs", string_of_int t.n;
	 "num_machines", string_of_int t.m; ] @ keys)
  in
    if overwrite || not (Sys.file_exists path) then write t path;
    path



(** {6 Taillard instances} ********************)

(** These instances are from:

    E. Taillard, "Benchmarks for Basic Scheduling Problems" *)

let bratley_random seed =
  (** [bratley_random seed] creates a new random number generator for
      floats in the range (0, 1).  This is the generator that Taillard
      uses for generating instances. *)
    let a = 16_807.
    and b = 127_773.
    and c = 2_836.
    and m = (2. ** 31.) -. 1.
    and seed = ref (float seed) in
    let next_float () =
      let k = floor (!seed /. b) in
	seed := a *. (mod_float !seed b) -. k *. c;
	if !seed < 0. then seed := !seed +. m;
	let f = !seed /. m in
	  f in
    let next_int a b =
      assert (a <= b);
      let af = float a and bf = float b in
      let f = next_float () in
	truncate (af +. f *. (bf -. af +. 1.))
    in next_float, next_int


let taillard_output chan t =
  (** [taillard_output chan t] outputs as two tables to [chan]. *)
  let n = t.n and m = t.m in
    Printf.printf "Processing times (d_ij):\n\t";
    for i = 0 to n - 1 do
      Printf.printf "%d\t" i
    done;
    Printf.printf "\n";
    for i = 0 to n - 1 do
      let ops = t.jobs.(i) in
	Printf.printf "%d\t" (i + 1);
	for j = 0 to m - 1 do
	  Printf.printf "%1.1d\t" ops.(j).duration
	done;
	Printf.printf "\n"
    done;
    Printf.printf "Machine times (M_ij):\n\t";
    for i = 0 to n - 1 do
      Printf.printf "%d\t" i
    done;
    Printf.printf "\n";
    for i = 0 to n - 1 do
      let ops = t.jobs.(i) in
	Printf.printf "%d\t" (i + 1);
	for j = 0 to m - 1 do
	  Printf.printf "%d\t" (ops.(j).machine + 1)
	done;
	Printf.printf "\n"
    done


let taillard ~n ~m ~time_seed ~machine_seed =
  (** [taillard ~n ~m ~machine_seed ~time_seed] makes an [n] job by [m]
      machine Taillard instance based on the given seeds. *)
  let _, next_time = bratley_random time_seed in
  let _, next_machine = bratley_random machine_seed in
  let jobs = Array.make_matrix n m null_operation in
    for i = 0 to n - 1 do
      let ops = jobs.(i) in
	for j = 0 to m - 1 do
	  ops.(j) <- { machine = j; duration = next_time 1 99; }
	done
    done;
    for i = 0 to n - 1 do
      let ops = jobs.(i) in
	for j = 0 to m - 1 do
	  let k = next_machine (j + 1) m in
	  let m_tmp = ops.(j).machine in
	    ops.(j).machine <- ops.(k - 1).machine;
	    ops.(k - 1).machine <- m_tmp
	done
    done;
    let t = make n m jobs in
      if Verb.level Verb.toplvl then taillard_output stdout t;
      t


let taillard_lower_bound t =
  (** [taillard_lower_bound t] computes Taillard's lower bound on the
      makespan for a jobshop problem. *)
  let max_job =
    Array.fold_left Math.imax 0
      (Array.map
	 (Array.fold_left (fun s o -> s + o.duration) 0) t.jobs)
  and max_mach =
    let bins = Hashtbl.create 100 in
      Wrarray.iteri_matrix (fun i j op ->
			      Hashtbl.add bins op.machine op.duration)
	t.jobs;
      List.fold_left Math.imax 0
	(List.map
	   (fun m ->
	      let ops = Hashtbl.find_all bins (m - 1) in
		List.fold_left (+) 0 ops)
	   (Wrlist.range t.m))
  in Math.imax max_job max_mach


let taillard_15_15 =
  [
    840612802,   398197754,    1247,    977;
    1314640371,   386720536,    1263,    942;
    1227221349,   316176388,    1233,    921;
    342269428,  1806358582,    1181,    911;
    1603221416,  1501949241,    1236,    940;
    1357584978,  1734077082,    1247,    889;
    44531661,  1374316395,    1235,    935;
    302545136,  2092186050,    1221,    963;
    1153780144,  1393392374,    1289,    982;
    73896786,  1544979948,    1270,    911;
  ]

let taillard_20_15 =
  [
    533484900,   317419073,    1376,    1139;
    1894307698,  1474268163,    1381,    1251;
    874340513,   509669280,    1368,    1178;
    1124986343,  1209573668,    1356,    1130;
    1463788335,   529048107,    1375,    1148;
    1056908795,    25321885,    1385,    1181;
    195672285,  1717580117,    1495,    1257;
    961965583,  1353003786,    1432,    1153;
    1610169733,  1734469503,    1378,    1202;
    532794656,   998486810,    1383,    1186;
  ]

let taillard_20_20 =
  [
    1035939303,   773961798,    1663,    1217;
    5997802,  1872541150,    1626,    1240;
    1357503601,   722225039,    1574,    1185;
    806159563,  1166962073,    1665,    1271;
    1902815253,  1879990068,    1598,    1256;
    1503184031,  1850351876,    1679,    1207;
    1032645967,    99711329,    1704,    1331;
    229894219,  1158117804,    1633,    1269;
    823349822,   108033225,    1635,    1267;
    1297900341,   489486403,    1616,    1212;
  ]

let taillard_10_10 =
  (* These are "open shop instances" *)
  [
    1344106948,  1868311537,     652,    637;
    425990073,  1111853152,     596,    588;
    666128954,  1750328066,     617,    598;
    442723456,  1369177184,     581,    577;
    2033800800,  1344077538,     657,    640;
    964467313,  1735817385,     545,    538;
    1004528509,   967002400,     623,    616;
    1667495107,   818777384,     606,    595;
    1806968543,  1561913259,     606,    595;
    938376228,   344628625,     604,    596;
  ]

let taillard_30_15 =
  [
    98640593,  1981283465,    1770,    1764;
    1839268120,   248890888,    1853,    1774;
    573875290,  2081512253,    1864,    1729;
    1670898570,   788294565,    1852,    1828;
    1118914567,  1074349202,    2015,    1729;
    178750207,   294279708,    1844,    1777;
    1549372605,   596993084,    1823,    1771;
    798174738,   151685779,    1714,    1673;
    553410952,  1329272528,    1824,    1641;
    1661531649,  1173386294,    1723,    1602;
  ]

let taillard_30_20 =
  [
    1841414609,  1357882888,    2064,    1830;
    2116959593,  1546338557,    1983,    1761;
    796392706,  1230864158,    1905,    1694;
    532496463,   254174057,    2031,    1787;
    2020525633,   978943053,    2038,    1731;
    524444252,   185526083,    2057,    1856;
    1569394691,   487269855,    1950,    1690;
    1460267840,  1631446539,    2014,    1744;
    198324822,  1937476577,    2013,    1758;
    38071822,  1541985579,    1973,    1674;
  ]

let taillard_50_15 =
  [
    17271,      718939,    2791,    2760;
    660481279,   449650254,    2800,    2756;
    352229765,   949737911,    2768,    2717;
    1197518780,   166840558,    2845,    2797;
    1376020303,   483922052,    2757,    2679;
    2106639239,   955932362,    2833,    2781;
    1765352082,  1209982549,    2977,    2943;
    1105092880,  1349003108,    2928,    2885;
    907248070,   919544535,    2722,    2655;
    2011630757,  1845447001,    2777,    2723;
  ]

let taillard_50_20 =
  [
    8493988,     2738939,    2961,    2868;
    1991925010,   709517751,    3013,    2848;
    342093237,   786960785,    2859,    2755;
    1634043183,   973178279,    2790,    2691;
    341706507,   286513148,    2813,    2725;
    320167954,  1411193018,    2921,    2845;
    1089696753,   298068750,    2907,    2812;
    433032965,  1589656152,    2840,    2764;
    615974477,   331205412,    3129,    3063;
    236150141,   592292984,    3173,    2995;
  ]

let taillard_100_20 =
  [
    302034063,  1203569070,    5582,    5464;
    1437643198,  1692025209,    5221,    5181;
    1792475497,  1039908559,    5671,    5552;
    1647273132,  1012841433,    5345,    5339;
    696480901,  1689682358,    5573,    5392;
    1785569423,  1092647459,    5403,    5342;
    117806902,   739059626,    5450,    5436;
    1639154709,  1319962509,    5459,    5394;
    2007423389,   749368241,    5360,    5358;
    682761130,   262763021,    5278,    5183;
  ]


let taillard_groups =
  [
    10, 10, taillard_10_10;
    15, 15, taillard_15_15;
    20, 15, taillard_20_15;
    20, 20, taillard_20_20;
    30, 15, taillard_30_15;
    30, 20, taillard_30_20;
    50, 15, taillard_50_15;
    50, 20, taillard_50_20;
    100, 20, taillard_100_20;
  ]


let save_taillard_insts () =
  List.iter
    (fun (n, m, insts) ->
       List.iter
	 (fun (tkey, mkey, _, lb) ->
	    let t = taillard n m tkey mkey in
	    let my_lb = taillard_lower_bound t in
	      if my_lb <> lb
	      then begin
		Printf.printf "Instance generation failed:\n";
		Printf.printf "n=%d, m=%d, tkey=%d, mkey=%d\n"
		  n m tkey mkey;
		Printf.printf "lb=%d, my lb = %d\n" lb my_lb;
		failwith "Bad instance"
	      end;
	      ignore (save t "taillard"
			["time seed", string_of_int tkey;
			 "machine seed", string_of_int mkey; ]))
	 insts)
    taillard_groups


let t_4_4 = lazy (taillard 4 4 1166510396 164000672)
  (** [t_4_4] an easy way to get Taillard's first 4x4 instance.

          |  Operation j
    Job i |  1   2   3   4
    ------+---------------
        1 | 54  34  61   2
        2 |  9  15  89  70
        3 | 38  19  28  87
        4 | 95  34   7  29
    =======================
          |  Operation j
    Job i |  1   2   3   4
    ------+---------------
        1 |  3   1   4   2
        2 |  4   1   2   3
        3 |  1   2   3   4
        4 |  1   3   2   4
  *)

(** {6 Random Instances} ****************************************)

let rand ~n ~m =
  (** [rand ~n ~m] makes a random instance using Taillard's
      procedure. *)
  taillard ~n ~m ~machine_seed:(Random.bits ()) ~time_seed:(Random.bits ())


let generate_rand ?(model="random") ~n ~m ~num =
  for i = 1 to num do
    let t = rand ~n ~m in
      ignore (save t model ["num", string_of_int i])
  done
