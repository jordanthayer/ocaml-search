(** Draws the roadmaps.

    @author eaburns
    @since 2010-08-17
*)

open Geometry
open Drawing
open Printf

let load_coordinates file =
  (** [load_coordinates file] loads the coordinates. *)
  Verb.pr Verb.always "Loading coordinates... %!";
  let c, time =
    Wrsys.with_time (fun () -> Dimacs.Binary_format.load_coordinates file)
  in
    Verb.pr Verb.always "done\n\t%d points (%f seconds)\n%!"
      c.Dimacs.c_nnodes time;
    c

module Spt = struct

  let points_of_coordinates c =
    (** [points_of_coordinates c] gets a points dataset from the given
	coordinates. *)
    let nnodes = c.Dimacs.c_nnodes in
    let points = Array.create nnodes (point nan nan) in
      Verb.pr Verb.always "Building dataset... %!";
      let ds, time =
	Wrsys.with_time
	  (fun () ->
	     for n = 1 to nnodes do
	       let sx = float (Dimacs.x_coord c n) in
	       let sy = float (Dimacs.y_coord c n) in
		 points.(n - 1) <- point sx sy;
	     done;
	     let points_ds =
	       Num_by_num.scatter_dataset ~point_radius:(Length.Pt 0.5)
		 Circle_glyph points
	     in points_ds)
      in
	Verb.pr Verb.always "done (%f seconds)\n" time;
	Verb.pr Verb.always "%d points\n" (Array.length points);
	ds


  let draw w h c outfile =
    let data = points_of_coordinates c in
    let plot = Num_by_num.plot [ data ] in
      if (Array.length Sys.argv) > 2
      then begin
	Verb.pr Verb.always "Saving plot to file %s... %!" outfile;
	let _, time = Wrsys.with_time (fun () -> plot#output outfile) in
	  Verb.pr Verb.always "done (%f seconds)\n%!" time;
      end;
      Verb.pr Verb.always "Displaying\n%!";
      plot#set_size ~w:(Length.Px w) ~h:(Length.Px h);
      plot#display

end

module Raw_eps = struct

  let buffer_ps_lines b lines =
    List.iter (fun s -> Buffer.add_string b s; Buffer.add_char b '\n') lines


  let buffer_ps_header title b w h =
    buffer_ps_lines b
      [
	(* Look at the ps-plot source for more on this... I have no idea
	   what some of it does. *)
	"%!PS-Adobe-3.0 EPSF-3.0";
	(sprintf "%%%%BoundingBox: 0 0 %d %d" w h);
	(sprintf "%%%%Title: %s" title);
	"%%Creator: drawmap by Ethan Burns (eaburns@unh.edu)";
	"%%CreationDate: Unknown";
	"%%Pages: 0";
	"%%Orientation: Portrait";
	"%%EndComments";
	"%%BeginProlog";
	"100 dict begin";
	"/bdef {bind def} bind def";
	"% sx sy ex ey L -   line from s to e";
	"/l {4 2 roll moveto lineto stroke} bdef";
	"% x y r D -   dot (filled circle)";
	"/d {newpath 0 360 arc fill} bdef";
	"% x y P -   point dot with set radius";
	"/p {0.001 newpath 0 360 arc fill} bdef";
	"gsave";
      ]


  let buffer_ps_footer b =
    buffer_ps_lines b [ "grestore"; "showpage"; "end"; ]


  let buffer_points b ~xmin ~xmax ~ymin ~ymax w h c =
    let rx = (float w) /. (float (xmax - xmin)) in
    let ry = (float h) /. (float (ymax - ymin)) in
    let output_point i j =
      let x = (float (i - xmin)) *. rx and y = (float (j - ymin)) *. ry in
	Buffer.add_string b (Printf.sprintf "%0.1f %0.1f p\n" x y)
    in
    let nnodes = c.Dimacs.c_nnodes in
      for n = 1 to nnodes do
	let i = Dimacs.x_coord c n in
	let j = Dimacs.y_coord c n in
	  output_point i j
      done

  let draw w h c outfile =
    if (Wrfname.get_extension outfile) <> ".eps"
    then invalid_arg "Outfile must have a .eps extension";
    let b = Buffer.create 4096 in
    let w = Math.round ((float w) *. (72. /. 96.)) in
    let h = Math.round ((float h) *. (72. /. 96.)) in
    let xmin, xmax, ymin, ymax = Dimacs.bounds c in
      buffer_ps_header outfile b w h;
      buffer_points b ~xmin ~xmax ~ymax ~ymin w h c;
      buffer_ps_footer b;
      Wrio.with_outfile outfile
	(fun outch -> output_string outch (Buffer.contents b))

end

module Image = struct

  open Eps
  open Ppm

  let point_color = 196, 196, 196

  let draw_coordinates img ~xmin ~xmax ~ymin ~ymax w h c =
    let rx = (float (w - 1)) /. (float (xmax - xmin)) in
    let ry = (float (h - 1)) /. (float (ymax - ymin)) in
    let nnodes = c.Dimacs.c_nnodes in
    let draw_point i j =
      let x = (float (i - xmin)) *. rx and y = (float (j - ymin)) *. ry in
      let clamp v = Math.imax 0 (Math.imin w (Math.round v)) in
	Image.draw_point img (clamp x) (clamp y) point_color;
    in
      for n = 1 to nnodes do
	let i = Dimacs.x_coord c n in
	let j = Dimacs.y_coord c n in
	  draw_point i j
      done


  let draw w h c outfile =
    let ftype = String.sub (Wrfname.get_extension outfile) 1 3 in
    let xmin, xmax, ymin, ymax = Dimacs.bounds c in
    let img = Image.create w h in
      draw_coordinates img ~xmin ~xmax ~ymin ~ymax w h c;
      Image.export ftype img outfile

end

let main () =
  let w = ref 960 in
  let h = ref 720 in
  let path = ref "" in
  let outfile = ref "" in
  let v = ref Verb.always in
  let spt = ref false in
  let eps = ref false in
  let image = ref false in
    Arg.parse [
      "-h", Arg.Set_int h, (sprintf "Image height in pixels (default: %d)" !h);
      "-w", Arg.Set_int w, (sprintf "Image width in pixels (default: %d)" !w);
      "-v", Arg.Set_int v, (sprintf "Verbosity (default: %d)" !v);

      "--use-spt", Arg.Set spt,
      "Use Spt (slow, graphical display, supports .pdf)";

      "--use-raw-eps", Arg.Set eps,
      "Output a raw .eps file (slow, large outpu file, good quality)";

      "--use-image", Arg.Set image,
      "Use raster image system (default: fastest, smallest, worst qual.)";
    ]
      (fun s ->
	 if !path = ""
	 then path := s
	 else (if !outfile = ""
	       then outfile := s
	       else invalid_arg s))
      "drawmap [-w <px>] [-h <px>] <map dir path> <outfile>";
    if !path = "" then failwith "Need to specify a path";
    if !outfile = "" then failwith "Need to specify a path";
    if (!spt && !eps) || (!spt && !image) || (!eps && !image)
    then invalid_arg "Must specify only a single backend";
    if (not !spt && not !eps && not !image) then image := true;
    Verb.with_level !v (fun () ->
			  let c = load_coordinates !path in
			    if !image then Image.draw !w !h c !outfile;
			    if !spt then Spt.draw !w !h c !outfile;
			    if !eps then Raw_eps.draw !w !h c !outfile;)

let _ = main ()
