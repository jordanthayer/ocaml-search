(** Shows what the colors on the grid represent
    Jordan - September 2009 *)

let width = 5

type t = {
  mutable context : int;
  mins  : float array;
  maxs  : float array;
  names : string array;
  mutable loc   : int * int;
  leng  : int;
}


let scale_window = ignore(GtkMain.Main.init());
  GWindow.window ~border_width:10 ~title:"Structure Display"
    ~deletable:false ~width:300 ~height:500 ()
let q = GMisc.drawing_area ~packing:scale_window#add ()
let w = q#misc#realize (); q#misc#window
let draw_area = new GDraw.drawable w


let reset_loc t sx sy =
  t.loc <- sx,sy


let getColor flt min max =
  Render.scale flt min max


let draw t =
  let sx,sy = t.loc
  and min = t.mins.(t.context)
  and max = t.maxs.(t.context) in
    draw_area#set_foreground `BLACK;
    draw_area#rectangle ~x:sx ~y:sy
      ~width:(width + 4 * Constants.offset) ~height:(t.leng + Constants.offset)
      ~filled:true ();
    (*draw_area#set_foreground `WHITE;
      draw_area#stringmoveto (sx + Constants.offset) sy;
      Graphics.draw_string (Wrutils.str "%s = %f" t.names.(t.context) min);
      Graphics.moveto (sx + Constants.offset) (sy + t.leng - (Constants.ts / 2));
      Graphics.draw_string (Wrutils.str "%s = %f" t.names.(t.context) max);*)
    (* now draw scale*)
    let delta = (max -. min) /. (float_of_int t.leng) in
      for i = 0 to t.leng
      do
	(draw_area#set_foreground
	   (`RGB (getColor (Render.Scaler
			      ((float_of_int i) *. delta +. min))
		    min max));
	 draw_area#rectangle ~x:sx ~y:(sy + i) ~width ~height:1
	   ~filled:true ())
      done


let cycle_context t =
  if t.context <> ((Array.length t.mins) - 1)
  then t.context <- t.context + 1
  else t.context <- 0;
  draw t


let set_context t i =
  t.context <- i

(* EOF *)
