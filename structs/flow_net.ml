(** A flow network.

    @author eaburns
    @since 2010-09-13
*)

module Arctbl = Hashtbl.Make(struct
			       type t = int * int
			       let equal (a0, a1) (b0, b1) =
				 (a0:int) = b0 && (a1:int) = b1
			       let hash = Hashtbl.hash
			     end)


type 'a arctbl = 'a Arctbl.t

type 'a garray = 'a Garray.t

type arc = {
  mutable flow : int;
  cap : int;
  src : int;
  dst : int;
}

type t = {
  source : int;
  sink : int;

  mutable nnodes : int;

  mutable arcs : arc list garray;
  (* outgoing arcs from src *)

  arctab : arc arctbl;
  (* table of arcs given (src * dst). *)
}


let create ~nnodes ~source ~sink =
  (** [create ~nnodes ~source ~sink] creates a new flow network. *)
  {
    source = source;
    sink = sink;
    nnodes = nnodes;
    arcs = Garray.make ~init_size:nnodes [];
    arctab = Arctbl.create (nnodes * 3);
  }


let add_node net =
  (** [add_node net] adds a node to the network. *)
  let node = net.nnodes in
    net.nnodes <- node + 1;
    node


let get_arc net ~src ~dst =
  (** [get_arc net ~src ~dst] gets the arc from the network if there
      is one, otherwise None. *)
  Arctbl.find net.arctab (src, dst)


let has_arc net ~src ~dst =
  (** [has_arc net ~src ~dst] test if the given arc exists in the
      network. *)
  Arctbl.mem net.arctab (src, dst)


let rec add_arc ?(antiparallel=false) net ~src ~dst ~cap =
  (** [add_arc ?antiparallel net ~src ~dst ~cap] adds an arc in the
      network.  If [antiparallel] is true then antiparallel edges are
      allowed, otherwise a dummy node may be added to the network. *)
  if not antiparallel && has_arc net ~src:dst ~dst:src then
    add_non_antiparallel net ~src ~dst ~cap
  else begin
    let a = { flow = 0; cap = cap; src = src; dst = dst } in
    let arcs = net.arcs in
    let outs = Garray.get arcs src in
      Garray.set arcs src (a :: outs);
      Arctbl.add net.arctab (src, dst) a
  end


and add_non_antiparallel net ~src ~dst ~cap =
  (** [add_non_antiparallel net ~src ~dst ~cap] add a dummy node with
      two arcs to avoid adding an antiparallel arc. *)
  let dummy = add_node net in
    add_arc ~antiparallel:true net ~src ~dst:dummy ~cap;
    add_arc ~antiparallel:true net ~src:dummy ~dst ~cap


let remove_arc net ~src ~dst =
  (** [remove_arc net ~src ~dst] removes an arc from the network. *)
  let arcs = net.arcs in
  let outs = Garray.get arcs src in
    Garray.set arcs src (List.filter (fun a -> a.dst <> dst) outs);
    Arctbl.remove net.arctab (src, dst)


let residual net =
  (** [residual net] gets a residual network. *)
  let r = create net.nnodes net.source net.sink in
    Arctbl.iter
      (fun (src, dst) arc ->
	 let flow = arc.flow in
	 let diff = arc.cap - flow in
	   if diff > 0 then
	     add_arc ~antiparallel:true r ~src ~dst ~cap:diff;
	   if flow > 0 then
	     add_arc ~antiparallel:true r ~src:dst ~dst:src ~cap:flow;)
      net.arctab;
    r


let output ch net =
  (** [output ch net] outputs the network to the given channel. *)
  Arctbl.iter (fun (s, d) a ->
		 Printf.fprintf ch "%d -> %d (%d / %d)\n"
		   s d a.flow a.cap)
    net.arctab


let augmenting_path net =
  (** [agumenting_path net] look for an augmenting path in the
      network. *)
  let rec handle_outarcs parents q ~sink ~n = function
    | { dst = n'; } :: rest when parents.(n') < 0 ->
	parents.(n') <- n;
	assert (parents.(n') >= 0);
	if n' <> sink then begin
	  Queue.add n' q;
	  handle_outarcs parents q ~sink ~n rest
	end
    | _ :: rest -> handle_outarcs parents q ~sink ~n rest
    | [] -> ()
  in
  let out_arcs = net.arcs and sink = net.sink and source = net.source in
  let parents = Array.create net.nnodes ~-1 in
  let rec build_path accum n =
    let p = parents.(n) in
      if p = n then n :: accum else build_path (n :: accum) p
  in
  let q = Queue.create () in
    parents.(source) <- source;
    Queue.add source q;
    while not (Queue.is_empty q) && parents.(sink) < 0 do
      let n = Queue.take q in
	handle_outarcs parents q ~sink ~n (Garray.get out_arcs n)
    done;
    if parents.(sink) < 0 then [] else build_path [] sink


let min_path_capacity net path =
  (** [min_path_capacity net path] gets the minimum capacity along a
      path. *)
  let rec get_min min = function
    | a :: ((b :: _) as rest) ->
	let a = get_arc net ~src:a ~dst:b in
	  get_min (Math.imin a.cap min) rest
    | _ -> min
  in
    match path with
      | a :: ((b :: _) as rest) ->
	  let a = try
	    get_arc net ~src:a ~dst:b
	  with Not_found ->
	    List.iter (Printf.printf " %d") path;
	    Printf.printf "\n\n";
	    output stdout net;
	    assert false
	  in
	    get_min a.cap rest
      | _ -> 0


let rec augment_path net c =
  (** [augment_path net c path] augments the edges on the path by
      the given capacity. *)
  function
    | a :: ((b :: _) as rest) ->
	if has_arc net ~src:a ~dst:b then begin
	  let a = get_arc net ~src:a ~dst:b in
	    a.flow <- a.flow + c;
	end else begin
	  let a = get_arc net ~src:b ~dst:a in
	    a.flow <- a.flow - c;
	end;
	augment_path net c rest
    | _ -> ()


let rec update_residual ~net ~res =
  (** [update_residual ~net ~res path] updates a residual network along
      the given path. *)
  function
    | a :: ((b :: _) as rest) ->
	let s, d = if has_arc net ~src:a ~dst:b then a, b else b, a in
	let arc = get_arc net ~src:s ~dst:d in
	let flow = arc.flow in
	let diff = arc.cap - flow in
	  if has_arc res ~src:s ~dst:d then remove_arc res ~src:s ~dst:d;
	  if has_arc res ~src:d ~dst:s then remove_arc res ~src:d ~dst:s;
	  if diff > 0 then
	    add_arc ~antiparallel:true res ~src:s ~dst:d ~cap:diff;
	  if flow > 0 then
	    add_arc ~antiparallel:true res ~src:d ~dst:s ~cap:flow;
	  update_residual ~net ~res rest
    | _ -> ()


let max_flow net =
  (** [max_flow net] computes the maximum flow in the network. *)
  Arctbl.iter (fun _ a -> a.flow <- 0) net.arctab;
  let res = residual net in
  let rec loop () =
    let path = augmenting_path res in
      if path <> [] then begin
	let c = min_path_capacity res path in
	  augment_path net c path;
	  update_residual ~net ~res path;
	  loop ()
      end
  in loop ()



let test_residual () =
  (** [test_residual ()] builds the flow network from CLRS edition 3
      fig 26.4 (a) and gets its residual (fig 26.4 (b)). *)
  let n = create ~nnodes:6 ~source:0 ~sink:5 in
  let arc n src dst flow cap =
    add_arc n src dst cap;
    let a = get_arc n src dst in
      a.flow <- flow
  in
    arc n 0 1 11 16;
    arc n 0 2 8 13;
    arc n 1 3 12 12;
    arc n 2 1 1 4;
    arc n 2 4 11 14;
    arc n 3 5 15 20;
    arc n 3 2 4 9;
    arc n 4 3 7 7;
    arc n 4 5 4 4;
    Printf.printf "net:\n";
    output stdout n;
    let res = residual n in
      Printf.printf "\nres:\n";
      output stdout res;
      let path = augmenting_path res in
	Printf.printf "path:";
	List.iter (Printf.printf " %d") path;
	Printf.printf "\n"



let test_flow () =
  (** [test_flow ()] builds the flow network from CLRS edition 3 fig
      26.6 and computes its maximum flow. *)
  let n = create ~nnodes:6 ~source:0 ~sink:5 in
    add_arc n 0 1 16;
    add_arc n 0 2 13;
    add_arc n 1 3 12;
    add_arc n 2 1 4;
    add_arc n 2 4 14;
    add_arc n 3 5 20;
    add_arc n 3 2 9;
    add_arc n 4 3 7;
    add_arc n 4 5 4;
    Printf.printf "net:\n";
    output stdout n;
    Printf.printf "\nmax flow:\n";
    max_flow n;
    output stdout n
