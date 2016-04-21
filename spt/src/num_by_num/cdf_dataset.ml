(**

    @author jtd7
    @since 2010-05-24
*)

open Num_by_num_dataset
open Drawing
open Geometry
open Statistics
open Verbosity

let f_compare a b =
  if (a:float) < b then -1 else if a = b then 0 else 1


(** [pts_to_accum normalize pts] gets the cumulative sums. *)
let pts_to_accum normalize pts =
  let accum = Array.map (fun p -> p.y) pts in
  let n = Array.length accum in
    for i = 1 to n - 1 do
      accum.(i) <- accum.(i) +. accum.(i - 1);
    done;
    let total = accum.(n - 1) in
      if normalize then
	for i = 0 to n - 1 do accum.(i) <- accum.(i) /. total done;
      accum


(** [cdf_samples normalize xmin xmax nsamples pts] computes the
    samples for a CDF given a range of x values.  [pts] is assumed
    to be sorted. *)
let cdf_samples normalize xmin xmax nsamples pts =
  (* We do sloppy comparisons here to fix floating point rounding
     issues.  This is OK because these lines will get clipped. *)
  let slop = (xmax -. xmin) /. 1_000_000_000_000. in
  let accum = pts_to_accum normalize pts in
  let n = Array.length accum in
  let delta = (xmax -. xmin) /. (float (nsamples - 1)) in
  let rec samples i num =
    assert (i < n);
    let x = xmin +. (delta *. num) in
      if Geometry.sloppy_float_leq ~slop x xmax then begin
	let i' = ref i in
	  while !i' < n && Geometry.sloppy_float_leq ~slop pts.(!i').x x do
	    incr i'
	  done;
	  decr i';
	  if !i' < 0 then
	    (point x 0.) :: samples i (num +. 1.)
	  else
	    (point x accum.(!i')) :: samples !i' (num +. 1.)
      end else
	[]
  in
  let ss = samples 0 0. in
    assert ((List.length ss) = nsamples);
    ss


class cdf_dataset
  dashes ?(nsamples=100) ?(normalize=false) ?(line_width=Length.Pt 1.)
  ?(color=black) ?name pts =

  let _ = Array.sort (fun a b -> f_compare a.x b.x) pts in

  let x_min, x_max, y_max =
    let xmin = float_ref infinity in
    let xmax = float_ref neg_infinity in
    let ysum = float_ref 0. in
      Array.iter (fun p ->
		    let x = p.x and y = p.y in
		      if x < !!xmin then xmin <-- x;
		      if x > !!xmax then xmax <-- x;
		      ysum <-- !!ysum +. y)
	pts;
      !!xmin, !!xmax, if normalize then 1. else !!ysum
  in

object(self)

  inherit dataset ?name ()

  val style = { line_color = color;
		line_dashes = dashes;
		line_width = line_width; }


  method dimensions = rectangle ~x_min ~x_max ~y_min:0. ~y_max


  method mean_y_value src =
    let pts = cdf_samples normalize src.x_min src.x_max nsamples pts in
    let s = List.fold_left (fun s p -> s +. p.y) 0. pts
    in s /. (float nsamples), nsamples


  method residual ctx ~src ~dst = zero_rectangle


  method draw ctx ~src ~dst =
    let pts = cdf_samples normalize src.x_min src.x_max nsamples pts in
    let tr = point_transform ~src ~dst in
      draw_line ctx ~box:src ~tr ~style pts


  method draw_legend ctx ~x ~y =
    let half_length = (ctx.units Line_dataset.line_legend_length) /. 2. in
    let x0 = x -. half_length and x1 = x +. half_length in
      draw_line ctx ~style [ point x0 y; point x1 y]


  method legend_dimensions ctx =
    (ctx.units Line_dataset.line_legend_length), (ctx.units line_width)


  method avg_slope = nan

end

let points_cdf_dataset dashes ?normalize ?nsamples ?line_width ?color
    ?name pts =
  new cdf_dataset dashes ?normalize ?nsamples ?line_width ?color ?name pts


let points_cdf_datasets ?normalize ?nsamples ?(color=false)
    name_by_points_list =
  let next_dash = Factories.default_dash_factory () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> black))
  in
    List.map (fun (name, points) ->
		points_cdf_dataset ?normalize ?nsamples ~color:(next_color ())
		  (next_dash ()) ~name points)
      name_by_points_list


let values_cdf_dataset dashes ?normalize ?nsamples ?line_width
    ?color ?name vls =
  let pts = Array.map (fun x -> point x 1.) vls in
    points_cdf_dataset ?normalize ?nsamples dashes ?line_width ?color ?name pts


let values_cdf_datasets ?normalize ?nsamples ?(color=false)
    name_by_values_list =
  let next_dash = Factories.default_dash_factory () in
  let next_color = (if color
		    then Factories.default_color_factory ()
		    else (fun () -> black))
  in
    List.map (fun (name, values) ->
		values_cdf_dataset ?normalize ?nsamples ~color:(next_color ())
		  (next_dash ()) ~name values)
      name_by_values_list


let cdf_dataset = values_cdf_dataset

let cdf_datasets = values_cdf_datasets


(* EOF *)
