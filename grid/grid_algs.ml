(* $Id: algs.ml,v 1.1 2006/09/02 00:49:51 ruml Exp ruml $
   connecting algorithms up to the search space
   Wooo!
*)

open Grid

(*** pretty-printing for screen ***)

let draw_board b =
  (** returns a string array *)
  let s = Array.init (width b) (fun _ -> String.make (height b) ' ') in
  let draw x y c =  s.(x).[y] <- c in
    for y = 0 to (height b) - 1 do
      for x = 0 to (width b) - 1 do
	if b.blocked.(x).(y) then
	  draw x y '#'
      done;
    done;
    draw (fst b.start) (snd b.start) 'S';
    List.iter (fun (x,y) -> draw x y 'G') b.goal;
    s


let draw_expands b pos =
  let n = List.length pos in
    Wrlist.iteri (fun i (x,y) ->
		    let i = (i * 10) / n in
		      b.(x).[y] <- Char.chr ((Char.code '0') + i))
      pos


let draw_path b s p =
  (** board -> string array -> (int * int) list -> unit *)
  let draw x y c =
    if ((x < 0) || (x >= (width b)) ||
	(y < 0) || (y >= (height b))) then
      failwith (Wrutils.str "bad path coordinate in %d,%d" x y);
    s.(x).[y] <- c
  in
    List.iter (fun (x,y) ->
		 draw x y 'o')
      (Wrlist.butlast (List.tl p))


let print_board ch s =
  (** out_channel -> string array -> unit *)
  for y = (String.length s.(0)) - 1 downto 0 do
    Array.iter (fun col -> output_char ch col.[y])
      s;
    Wrutils.newline ch
  done;
  flush ch


let print_results w s e g p m d t =
  (** world, sol, expand, gen, time *)
  let b = draw_board w in
    draw_expands b (get_cache ());
    (match s with
       None ->
	 Wrutils.pr "No feasible path in board:\n";
	 print_board stdout b
     | Some (p, c) ->
	 let l = (List.length p) - 1 in
	   draw_path w b p;
	   Wrutils.pr "Found path of length %d with cost %d in %d expansions:\n" l (truncate c) e;
	   print_board stdout b;
	   Wrutils.pr "Expanded/path length = %.2f\n" (Math.div e l));
    Wrutils.pr "Search took %.3f seconds.\n" t;
    Wrutils.pr "%d nodes expanded (%d per second)\n"
      e (Math.round ((float e) /. t));
    Wrutils.pr "%d nodes generated (%d per second)\n"
      g (Math.round ((float g) /. t));
    Wrutils.pr "Branching factor of %f.\n" (Math.div g e);
    Wrutils.pr "%d nodes pruned.\n" p;
    Wrutils.pr "%d duplicate nodes detected.\n" d;
    Wrutils.pr "Open list reached a maximum size of %d nodes.\n" m;
    Wrutils.pr "%!"


(******************** search interface *********************)
(**
   s:
   e:
   g:
   p:
   m:
   d:
*)


let with_path6 (s, e, g, p, m, d) =
  (match s with
     | None -> None
     | Some (n, f) -> Some (path n, f)), e, g, p, m, d

let with_path3 (s, e, g) =
  with_path6 (s, e, g, 0, 0, 0)

let with_path4 (s, e, g, d) =
  with_path6 (s, e, g, 0, 0, d)


let debug_logger w =
  let start = Sys.time () in
    (fun n f i ->
       print_results w (Some (path n, f))
	 i.Limit.expanded i.Limit.generated i.Limit.pruned
	 i.Limit.max_q i.Limit.duplicates ((Sys.time ()) -. start))


let make_logger () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time ()
  and threshold = ref infinity in
    (fun n f i ->
       (** cost, length, expanded, gen, time *)
       if f <= !threshold then
	 let t = (Sys.time ()) -. start
	 and l = List.length (path n) in
	   Wrutils.pr "%f\t%d\t%d\t%d\t%f\n%!"
	     f l i.Limit.expanded i.Limit.generated t;
	   threshold := f *. 0.999)

let make_inc_logger () =
  (** prevent anytime A* from writing thousands of lines - only write if
    improved significantly *)
  let start = Sys.time () in
    (fun n f exp gen ->
       (** cost, length, expanded, gen, time *)
       let t = (Sys.time ()) -. start
       and l = List.length (path n) in
	 Wrutils.pr "%f\t%d\t%d\t%d\t%f\n%!"
	   f l exp gen t)
(* EOF *)
