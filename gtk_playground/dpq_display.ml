(* For Displaying a dpq *)

type 'a t = {
  name : string;
  mutable sx : int;
  mutable sy : int;
  iSize: int;
  queue : 'a Structure.t;
  drawing : GDraw.drawable;
}

let ts = 20


let dpq_window = ignore(GtkMain.Main.init());
  GWindow.window ~border_width:10 ~title:"Structure Display"
    ~deletable:false ~width:800 ~height:50 ()
let q = GMisc.drawing_area ~packing:dpq_window#add ()
let w = q#misc#realize (); q#misc#window

let q_count = ref 0

let new_dispq name queue =
  q_count := !q_count + 1;
    {name = name;
      sx = 0;
      sy = !q_count * ts;
      iSize = 5;
      queue = queue;
      drawing = new GDraw.drawable w }

let getColor flt min max =
  (** Gets the color of a given float between the minimum and maximum value *)
  (*Render.scale flt min max*)
  (*(Gdk.Color.alloc ~colormap:(Gdk.Rgb.get_cmap()))*) (`RGB (65535,0,0))

let clear dq =
  dq.drawing#set_foreground `BLACK;
  dq.drawing#rectangle ~x:dq.sx ~y:dq.sy ~width:800 ~height:ts
    ~filled:true ()

let draw dq gc (run,step) min max =
  (** Draws a dpq based on the current context *)
  clear dq;
  Array.iteri
    (fun i e ->
       dq.drawing#set_foreground (getColor (gc e step) min max);
       dq.drawing#rectangle
	 ~x:(dq.sx + i * dq.iSize) ~y:dq.sy ~width:dq.iSize ~height:ts
	 ~filled:true ())
    (Structure.get_entry dq.queue step).Structure.keys;


(* EOF *)
