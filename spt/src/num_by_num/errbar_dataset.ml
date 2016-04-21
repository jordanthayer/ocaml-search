(**

    @author eaburns
    @since 2010-04-28
*)


open Geometry
open Drawing


(** A dataset that consists of a bunch of error bars. *)
class virtual errbar_dataset triples =
object
  inherit Num_by_num_dataset.dataset ()

  val triples = (triples : triple array)
    (* point and magnitude. *)
end

(** {1 Vertical error bars} *)

(** A set of vertical error bars. *)
class vertical_errbar_dataset ?color triples =
  let style = match color with
    | None -> Errbar.errbar_line_style
    | Some color -> { Errbar.errbar_line_style with line_color = color }
  in
object (self)
  inherit errbar_dataset triples

  method residual ctx ~src ~dst =
    let tr = range_transform ~src:(xrange src) ~dst:(xrange dst) in
      Array.fold_left
	(fun r t ->
	   if rectangle_contains src (point t.i t.j)
	   then begin
	     let src_y = yrange src and dst_x = xrange dst in
	     let x = tr t.i and y = t.j and mag = t.k in
	     let up_residue =
	       Errbar.residual_vert ctx true ?cap_size:None
		 ~src_y ~dst_x ~x ~y ~mag
	     and down_residue =
	       Errbar.residual_vert ctx false ?cap_size:None
		 ~src_y ~dst_x ~x ~y ~mag
	     in
	       rectangle_max r (rectangle_max up_residue down_residue)
	   end else r)
	zero_rectangle triples


  method dimensions =
    Array.fold_left
      (fun r t ->
	 let low = t.j -. t.k and high = t.j +. t.k in
	 let x = t.i in
	 let rect = rectangle ~x_min:x ~x_max:x ~y_min:low ~y_max:high
	 in rectangle_extremes r rect)
      (rectangle
	 ~x_min:infinity ~x_max:neg_infinity
	 ~y_min:infinity ~y_max:neg_infinity)
      triples


  method mean_y_value _ = nan, 0


  (** [draw ctx ~src ~dst] draws the data to the plot. *)
  method draw ctx ~src ~dst =
    let tr = range_transform ~src:(xrange src) ~dst:(xrange dst) in
      Array.iter (fun t ->
		    let xrng = xrange dst in
		    let src = yrange src and dst = yrange dst in
		    let x = tr t.i and y = t.j and mag = t.k in
                      if x <= xrng.max && x >= xrng.min then begin
		      	Errbar.draw_up ctx ~style ?cap_size:None
				~src ~dst ~x ~y ~mag;
		     	 Errbar.draw_down ctx ~style ?cap_size:None
				~src ~dst ~x ~y ~mag
                       end)
	triples


  method draw_legend _ ~x:_ ~y:_ = ()

  method legend_dimensions _ = 0., 0.

  method avg_slope = nan

end


(** {1 Horizontal error bars} *)

(** A set of horizontal error bars. *)
class horizontal_errbar_dataset ?color triples =
  let style = match color with
    | None -> Errbar.errbar_line_style
    | Some color -> { Errbar.errbar_line_style with line_color = color }
  in
object (self)
  inherit errbar_dataset triples

  method residual ctx ~src ~dst =
    let tr = range_transform ~src:(yrange src) ~dst:(yrange dst) in
      Array.fold_left
	(fun r t ->
	   if rectangle_contains src (point t.i t.j)
	   then begin
	     let src_x = xrange src and dst_y = yrange dst in
	     let x = t.i and y = tr t.j and mag = t.k in
	     let left_residue =
	       Errbar.residual_horiz ctx true ?cap_size:None
		 ~src_x ~dst_y ~x ~y ~mag
	     and right_residue =
	       Errbar.residual_horiz ctx false ?cap_size:None
		 ~src_x ~dst_y ~x ~y ~mag
	     in
	       rectangle_extremes r (rectangle_extremes
				       left_residue right_residue)
	   end else r)
	zero_rectangle triples


  method dimensions =
    Array.fold_left
      (fun r t ->
	 let low = t.i -. t.k and high = t.i +. t.k in
	 let y = t.j in
	 let rect = rectangle ~x_min:low ~x_max:high ~y_min:y ~y_max:y
	 in rectangle_extremes r rect)
      (rectangle
	 ~x_min:infinity ~x_max:neg_infinity
	 ~y_min:infinity ~y_max:neg_infinity)
      triples


  method mean_y_value _ = nan, 0


  (** [draw ctx ~src ~dst] draws the data to the plot. *)
  method draw ctx ~src ~dst =
    let tr = range_transform ~src:(yrange src) ~dst:(yrange dst) in
      Array.iter (fun t ->
		    let src = xrange src and dst = xrange dst in
		    let x = t.i and y = tr t.j and mag = t.k in
		      Errbar.draw_left ctx ~style ?cap_size:None
			~src ~dst ~x ~y ~mag;
		      Errbar.draw_right ctx ~style ?cap_size:None
			~src ~dst ~x ~y ~mag;)
	triples


  method draw_legend _ ~x:_ ~y:_ = ()

  method legend_dimensions _ = 0., 0.

  method avg_slope = nan

end


let vert_errbar_dataset ?color triples =
  let ts =
    Array.map
      (fun t ->
	 let x = t.i and low = t.j and high = t.k in
	 let mag = (high -. low) /. 2. in
	 let y = mag +. low in
	   triple x y mag)
      triples
  in
    new vertical_errbar_dataset ?color ts


(* EOF *)
