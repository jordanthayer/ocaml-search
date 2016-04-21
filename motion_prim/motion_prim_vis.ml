
(*
 * Draw the arc traversed by the motion primitive on screen.
 *
 * @param [px_res] the resolution in pixels per meter (float).
 * @param [color] the color to draw the arc (int).
 * @param [mp] the motion primitive to draw (Motion_primitive.motion_primitive).
 *)
let draw_mp_arc px_res (ox,oy) color mp =
  let pts = Motion_prim.get_xyh_array mp in
  let xy_pts = Array.map (fun (x,y,_) -> (ox + (Math.round (px_res *. x)),
                                          oy + (Math.round (px_res *. y))))
                 pts in 
    Array.iter (fun (a,b) -> Printf.printf "(%d. %d)\n%!" a b) xy_pts;
    Graphics.draw_poly_line xy_pts

(*
 * Draw a circle at the end of the motion primitive with a heading indicator.
 *
 * @param [px_res] the resolution in pixels per meter (float).
 * @param [color1] the color to draw the circle (int).
 * @param [color2] the color to draw the heading indicator (int).
 * @param [mp] the motion primitive (Motion_primitive.motion_primitive).
 *)
let draw_end_heading px_res (ox,oy) color1 color2 mp =
  let circle_radius = 5 in (* size of end circle in pixels *)
  let line_dist = 10 in
  let x,y,h = Motion_prim.get_xyh_end mp in
  let px = ox + (Math.round (px_res *. x)) in
  let py = oy + (Math.round (px_res *. y)) in
    Graphics.set_color color1;
    Graphics.fill_ellipse px py circle_radius circle_radius;
  let ex = Math.round ((float line_dist) *. (cos h)) in
  let ey = Math.round ((float line_dist) *. (sin h)) in
    Graphics.set_color color2;
    Graphics.moveto px py;
    Graphics.rlineto ex ey


(*
 *
 *)
let draw_motion_prim ?(arc_c=Graphics.blue) ?(circ_c=Graphics.yellow)
      ?(head_c=Graphics.red) ?(heading=true) px_res origin mp = 
  draw_mp_arc px_res origin arc_c mp;
  if heading then draw_end_heading px_res origin circ_c head_c mp


(*
 *
 *)
let visualize_mps px_res mps = 
  let width = 4.0
  and height = 4.0 in
  let w = Math.round (px_res *. width)
  and h = Math.round (px_res *. height) in
  let origin = (w/2, h/2) in
    Graphics.open_graph "";
    Graphics.resize_window w h;
    Graphics.set_color Graphics.blue;
    List.iter (fun mp -> draw_motion_prim px_res origin mp) mps;
    Printf.fprintf stderr "click mouse to exit\n%!";
    Graphics.wait_next_event [Graphics.Button_down]




(* EOF *)

