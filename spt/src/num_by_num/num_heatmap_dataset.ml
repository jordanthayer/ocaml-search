(**

    @author jtd7
    @since 2010-05-27

   Numeric by Numeric Heatmap Dataset
*)

open Num_by_num_dataset
open Drawing
open Geometry

let default_bins = 10.


(** the index of the bin that should contain [value].  Note that any
    value outside the range of representable integers, such as
    [infinity], may yield a garbage value, such as 0!  If you might
    pass such a value, test before calling!  (This routine is
    intended to be simple and fast.) *)
let bucket ~min ~bin_size value =
  truncate ((value -. min) /. bin_size)


let make_default_gradient min max =
  let range = max -. min in
    (fun value ->
       let src = (value /. range) *. 3. in
	 if src < 1. then { r = src; g = 0.; b= 0.; a = 1.; }
	 else if src < 2. then { r = 1.; g = src -. 1.; b = 0.; a = 1.}
	 else {r = 1.; g = 1.; b = src -. 2.; a = 1.})


let make_default_gradient_rg min max =
  let range = max -. min in
    (fun value ->
       let src = ((value -. min) /. range)  in
	 { r = src; g = (1.0 -. src); b = 0.; a = 1.; })

let make_default_gradient_bw min max =
  let range = max -. min in
    (fun value ->
       let src = ((value -. min) /. range)  in
	 { r = src; g = src; b = src; a = 1.; })



class countmap_dataset ?(line_width=Length.Pt 1.) ?bin_size ?gradient points =

  let x_min, y_min, x_max, y_max =
    Array.fold_left
      (fun (a_xmin,a_ymin,a_xmax, a_ymax) t ->
	 min a_xmin t.x, min a_ymin t.y, max a_xmax t.x, max a_ymax t.y)
      (infinity,infinity,neg_infinity,neg_infinity) points in

  let bin_width = (match bin_size with
		       None -> (x_max -. x_min) /. default_bins
		     | Some pt -> pt.x)

  and bin_height = (match bin_size with
			None -> (y_max -. y_min) /. default_bins
		      | Some pt -> pt.y) in

  let bins = (let xcnt = truncate (ceil ((x_max -. x_min) /. bin_width))
	      and ycnt = truncate (ceil ((y_max -. y_min) /. bin_height)) in
	      let b = Array.create_matrix xcnt ycnt 0. in
		Array.iter
		  (fun pt ->
		     let xi = bucket ~min:x_min ~bin_size:bin_width pt.x
		     and yi = bucket ~min:x_min ~bin_size:bin_height pt.y in
		     let xi = min xi (xcnt - 1)
		     and yi = min yi (ycnt - 1) in
		       b.(xi).(yi) <- b.(xi).(yi) +. 1.) points;
		b) in

  let gradient = (match gradient with
		      None -> make_default_gradient 0.
			(Array.fold_left
			   (fun accum ar ->
			      max accum (Array.fold_left max neg_infinity ar))
			   neg_infinity bins)
		    | Some fn -> fn) in

object (self)

  inherit dataset ()

  method dimensions =
    rectangle ~x_min ~x_max:(x_min +. bin_width *. (float (Array.length bins)))
      ~y_min ~y_max:(y_min +. bin_height *. (float (Array.length bins.(0))))

  method mean_y_value _ = nan, 0

  method draw ctx ~src ~dst =
    let tr_rect = rectangle_transform ~src ~dst in
      for x = 0 to (Array.length bins - 1) do
	for y = 0 to (Array.length bins.(0) - 1) do
	  (let x_min = x_min +. (float x) *. bin_width
	   and y_min = y_min +. (float y) *. bin_height
	   and x_max = x_min +. (float (x+1)) *. bin_width
	   and y_max = y_min +. (float (y+1)) *. bin_height in
	     fill_rectangle ctx ~color:(gradient bins.(x).(y))
	       (tr_rect (rectangle ~x_min ~x_max ~y_min ~y_max)))
	done
      done

  method avg_slope =
    nan

  method draw_legend ctx ~x ~y = ()

  method legend_dimensions ctx =
    0., (ctx.units line_width)

end


let countmap_dataset ?(line_width = Length.Pt 1.) ?bin_size ?gradient points =
  new countmap_dataset ~line_width ?bin_size ?gradient points




class valuemap_dataset
  ?(line_width=Length.Pt 1.) ?bin_size ?gradient ?name triples=


  let x_min, y_min, x_max, y_max =
    Array.fold_left
      (fun (a_xmin,a_ymin,a_xmax, a_ymax) t ->
	 min a_xmin t.i, min a_ymin t.j, max a_xmax t.i, max a_ymax t.j)
      (infinity,infinity,neg_infinity,neg_infinity) triples in

  let bin_width = (match bin_size with
		       None -> (x_max -. x_min) /. default_bins
		     | Some pt -> pt.x)

  and bin_height = (match bin_size with
			None -> (y_max -. y_min) /. default_bins
		      | Some pt -> pt.y) in

  let bins = (let xcnt = truncate (ceil ((x_max -. x_min) /. bin_width))
	      and ycnt = truncate (ceil ((y_max -. y_min) /. bin_height)) in
	      let b = Array.create_matrix (xcnt+1) (ycnt+1) 0. in

		Array.iter
		  (fun trp ->
		     let xi = bucket ~min:x_min ~bin_size:bin_width trp.i
		     and yi = bucket ~min:y_min ~bin_size:bin_height trp.j in
		     let xi = min xi (xcnt)
		     and yi = min yi (ycnt) in
		       b.(xi).(yi) <- b.(xi).(yi) +. trp.k) triples;
		b) in

  let gradient = (match gradient with
		      None -> make_default_gradient_bw
			(Array.fold_left
			   (fun accum ar ->
			      min accum (Array.fold_left min infinity ar))
			   infinity bins)
			(Array.fold_left
			   (fun accum ar ->
			      max accum (Array.fold_left max neg_infinity ar))
			   neg_infinity bins)
		    | Some fn -> fn) in

object (self)

  inherit dataset ()

  method dimensions =
    rectangle ~x_min ~x_max:(x_min +. bin_width *. (float (Array.length bins)))
      ~y_min ~y_max:(y_min +. bin_height *. (float (Array.length bins.(0))))

  method mean_y_value _ = nan, 0

  method draw ctx ~src ~dst =
    let tr_rect = rectangle_transform ~src ~dst in
      for x = 0 to (Array.length bins - 1) do
	for y = 0 to (Array.length bins.(0) - 1) do
	  (let x_min = x_min +. (float x) *. bin_width
	   and y_min = y_min +. (float y) *. bin_height
	   and x_max = x_min +. (float (x+1)) *. bin_width
	   and y_max = y_min +. (float (y+1)) *. bin_height in
	     fill_rectangle ctx ~color:(gradient bins.(x).(y))
	       (tr_rect (rectangle ~x_min ~x_max ~y_min ~y_max)))
	done
      done

  method avg_slope =
    nan

  method draw_legend ctx ~x ~y = ()

  method legend_dimensions ctx =
    0., (ctx.units line_width)

end


let valuemap_dataset ?(line_width = Length.Pt 1.) ?bin_size ?gradient triples=
  new valuemap_dataset ~line_width ?bin_size ?gradient triples

let heatmap_dataset = valuemap_dataset

(* EOF *)
