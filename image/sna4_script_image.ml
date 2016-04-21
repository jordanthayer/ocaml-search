(**

   @author sna4
   @since 2010-02-25
*)

(* pulling in things we need to be able to build DTA* learning
   table *)
type prob_table = Dtastar_old.prob_table
let load_table = Dtastar_old.load_table
let get_counts = Dtastar_old.get_counts

open Eps

let get_vals tbl x =
  let n = float (Hashtbl.find tbl x) in
    ((n*.(float x)), (n))

let fmx l =
  (List.fold_left Math.fmax (List.hd l) l)

let fmn l =
  (List.fold_left Math.fmin (List.hd l) l)

let mx l =
  (List.fold_left max (List.hd l) l)

let mn l =
  (List.fold_left min (List.hd l) l)

let get_color_val min max num =
  let num =
    if num > max then max else if num < min then min else num in
  let num = (((num -. min) /. (max -. min)) *. 510.) in
    assert (num >= 0.);
    assert (num <= 510.);
    let r,g,b =
      (let v1 = int_of_float num in
	 Verb.pe Verb.debug "v: %i\n" v1;
	 (* from 125 0 0 to 255 255 125 *)
	 if v1 <= 125
	 then (255, 255, (125 - v1))
	 else
	   (if v1 <= 380
	    then (255, (255 - (v1 - 125)), 0)
	    else ((255 - (v1 - 380)), 0, 0))) in
      (r, g, b)

let output_avg_error path accuracy horizon =
  (** Generates a table displaying h-value as the y axis, d-value as
      the x axis, and avg heuristic error as a color scaled value. *)
  let t = load_table path in
  let h_vals = Wrht.keys t.Dtastar_old.vcounts in
  let err_avgs = ref []
  and err_nums = ref []
  and err_vars = ref []
  and err_draw_funcs = ref []
  and num_draw_funcs = ref []
  and var_draw_funcs = ref []
  and err_max = ref 0. in
  let inc = 1. /. (10.**(float accuracy)) in
  let hmx = fmx h_vals
  and hmn = fmn h_vals in
  let img_err = Image.create horizon (truncate (ceil hmx))
  and img_var = Image.create horizon (truncate (ceil hmx))
  and img_num = Image.create horizon (truncate (ceil hmx)) in
  let h = ref hmn in
    ((while !h <= hmx do
	((for d = 1 to horizon do
	    let s = ref 0.
	    and n = ref 0
	    and err_tbl = get_counts t !h d in
	      Hashtbl.iter (fun k v -> s := !s +. (k*.(float v));
			      n := !n + v;
			      err_max := (Math.fmax !err_max k)) err_tbl;
	      let err_avg = (!s/.(float !n)) in
	      let v = (Hashtbl.fold
			 (fun err_val count acc ->
			    (acc+.((err_val-.err_avg)**2.)*.
			       (float count))) err_tbl 0.)/.
		(float !n) in
		err_draw_funcs := (Image.draw_point img_err (d-1)
				     (truncate (!h-.1.)))::!err_draw_funcs;
		num_draw_funcs := (Image.draw_point img_num (d-1)
				     (truncate (!h-.1.)))::!num_draw_funcs;
		var_draw_funcs := (Image.draw_point img_var (d-1)
				     (truncate (!h-.1.)))::!var_draw_funcs;
		err_avgs := err_avg::!err_avgs;
		err_nums := !n::!err_nums;
		err_vars := v::!err_vars;
		Printf.printf "h:%f d:%d sum:%f num:%d avg:%f variance:%f\n"
		  !h d !s !n err_avg v
	  done);
	 h := !h +. inc;)
      done);
     (let get_color = (get_color_val
			 (fmn !err_avgs)
			 (fmx !err_avgs)) in
	(List.iter2 (fun f err_avg ->
		       f (get_color err_avg)) !err_draw_funcs !err_avgs);
	Image.export "eps" img_err "./sna4/error.eps");
     (let get_color = (get_color_val
			 (float (mn !err_nums))
			 (float (mx !err_nums))) in
	(List.iter2 (fun f err_num ->
		       f (get_color (float err_num))) !num_draw_funcs !err_nums);
	Image.export "eps" img_num "./sna4/counts.eps");
     (let get_color = (get_color_val
			 (fmn !err_vars)
			 (fmx !err_vars)) in
	(List.iter2 (fun f err_var ->
		       f (get_color err_var)) !var_draw_funcs !err_vars);
	Image.export "eps" img_var "./sna4/variance.eps");
     Printf.printf "max error: %f\n" !err_max;
     Printf.printf "max error avg: %f\n" (fmx !err_avgs);
     Printf.printf "max count: %d\n" (mx !err_nums);
     Printf.printf "max variance: %f\n" (fmx !err_vars);
     Printf.printf "min error avg: %f\n" (fmn !err_avgs);
     Printf.printf "min count: %d\n" (mn !err_nums);
     Printf.printf "min variance: %f\n" (fmn !err_vars);)
