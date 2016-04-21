(* For Displaying a dpq *)
open Recorded_run

type 'a t = {
  name : string;
  sx : int;
  sy : int;
  iSize: int;
  queue : 'a Structure.t;
  drawing : GDraw.drawable;
}


let dpq_window = ignore(GtkMain.Main.init());
  GWindow.window ~border_width:10 ~title:"Open"
    ~deletable:false ~width:800 ~height:100 ()
let q = GMisc.drawing_area ~packing:dpq_window#add ()
let w = (q#misc#realize (); q#misc#window)
let pmap = ref (GDraw.pixmap ~width:800 ~height:Constants.ts ())
let last_size = ref 1


let new_dispq i name queue =
  Verb.pe Verb.always "%i\n" i;
  let f = {name = name;
      sx = 0;
      sy = (i * Constants.ts);
      iSize = 5;
      queue = queue;
      drawing = new GDraw.drawable w } in
    !pmap#set_background `BLACK;
    f.drawing#set_background `BLACK;
    f

let getColor flt min max =
  (** Gets the color of a given float between the minimum and maximum value *)
  `RGB (Render.scale flt min max)


let clear dq size =
  last_size := (max !last_size size);
  pmap := GDraw.pixmap ~width:!last_size ~height:Constants.ts ();
  !pmap#set_foreground `BLACK;
  !pmap#rectangle ~x:dq.sx ~y:dq.sy ~width:!last_size ~height:Constants.ts
    ~filled:true ()


let draw dq gc (run,step) min max =
  (** Draws a dpq based on the current context *)
  let queue = Structure.get_entry dq.queue step in
  let size = (Array.length queue.Structure.keys) * dq.iSize in
  clear dq size;
  Array.iteri
    (fun i e ->
       !pmap#set_foreground (getColor (gc e step) min max);
       !pmap#rectangle
	 ~x:(dq.sx + i * dq.iSize) ~y:dq.sy ~width:dq.iSize ~height:Constants.ts
	 ~filled:true ())
    queue.Structure.keys;
  dq.drawing#put_pixmap ~x:dq.sx ~y:dq.sy ~xsrc:dq.sx ~ysrc:dq.sy
    ~width:!last_size ~height:Constants.ts !pmap#pixmap


(* EOF *)
