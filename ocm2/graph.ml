(** Some routines for graphs.

    @author eaburns
    @since 2010-12-07
*)

open Verb

module type Digraph = sig
  type graph
  type node
  val iter : graph -> (node -> unit) -> unit
  val succs : graph -> node -> node list
  val equal : node -> node -> bool
  val hash : node -> int
  val print_node : out_channel -> node -> unit
end

module Algs (G : Digraph) : sig

  exception Cycle

  (** Topologically sorts the given graph.

      @param allow_cycles If it is true then don't error if a cycle is
      found, instead break it and continue.  If false, the raise a Cycle
      exception.  *)
  val topo_sort : ?allow_cycles:bool -> G.graph -> G.node list

  (** Gets an array of nodes with their heights from leaf nodes. *)
  val topo_heights : G.graph -> G.node list array

end = struct

  module Ht = Hashtbl.Make(struct include G type t = node end)

  exception Cycle

  let topo_sort ?(allow_cycles=false) g =
    let lst = ref [] in
    let seen = Ht.create 149 in
    let path = Ht.create 149 in
    let rec visit n =
      if Ht.mem path n then begin
	if not allow_cycles then raise Cycle;
      end else
	if not (Ht.mem seen n) then begin
	  Ht.add seen n true;
	  Ht.add path n true;
	  List.iter visit (G.succs g n);
	  Ht.remove path n;
	  lst := n :: !lst
	end
    in
      G.iter g visit;
      !lst


  (** Counts the number of nodes in the graph. *)
  let nnodes g =
    let c = ref 0 in
      G.iter g (fun _ -> incr c);
      !c


  let topo_heights g =
    let seen = Ht.create 149 in
    let max_ht = ref ~-1 in
    let hts = Array.create (nnodes g) [] in
    let rec height n =
      try
	Ht.find seen n
      with Not_found ->
	let ht = match G.succs g n with
	  | [] -> 0
	  | succs ->
	      (List.fold_left (fun h n -> max h (height n)) ~-1 succs) + 1
	in
	  if ht > !max_ht then max_ht := ht;
	  assert (ht < nnodes g);
	  hts.(ht) <- n :: hts.(ht);
	  Ht.add seen n ht;
	  ht
    in
      G.iter g (fun n -> ignore (height n));
      Array.sub hts 0 (!max_ht + 1)

end
