(** The layout of the manufacturing plant along with functions for
    instance I/O and creating random instances. *)

open Printf
open Scanf
open Fn

type mach_id = int
type attr_id = int

type attr = {
  attr_id : attr_id;
  proc_time : int;
}

type transport = {
  dest_id : mach_id;
  trans_time : int;
}

type machine = {
  mach_id : mach_id;
  attrs : attr array;
  out : transport array;
}

type layout = {
  nattrs : int;
  nmachs : int;
  machs : machine array;
  inputs : mach_id array;
  outputs : mach_id array;
}

type inst = {
  layout : layout;
  nmaterial : int;
  srcs : int array;
  dests : int array;
  desired_attrs : int list array;
}

(* Sample layout from "A Simple Testbed for On-Line Planning", Benton,
   Do and Ruml.  I made up the transport and processing times for this
   example because I couldn't find them with a quick glance over the
   paper. *)
let j_sample_layout =
  { nattrs = 2;
    nmachs = 6;
    machs = [| { mach_id = 0; attrs = [||];
                 out = [| { dest_id = 2; trans_time = 2 }; |]; };
               { mach_id = 1; attrs = [||];
                 out = [| { dest_id = 3; trans_time = 2 }; |]; };
               { mach_id = 2; attrs = [| { attr_id = 0; proc_time = 10 }; |];
                 out = [| { dest_id = 4; trans_time = 2 };
                          { dest_id = 5; trans_time = 2 }; |]; };
               { mach_id = 3; attrs = [| { attr_id = 1; proc_time = 10 }; |];
                 out = [| { dest_id = 2; trans_time = 2 } |]; };
               { mach_id = 4; attrs = [||]; out = [||]; };
               { mach_id = 5; attrs = [||]; out = [||]; }; |];
    inputs = [| 0; 1 |];
    outputs = [| 4; 5 |];
  }

let example =
  { layout = j_sample_layout;
    nmaterial = 2;
    srcs = [| 1; 0 |];
    dests =  [| 4; 5; |];
    desired_attrs = [| [1]; [0] |]; }

let valid_input_mach inst id =
  let leads_to id m = Wrarray.exists (fun t -> t.dest_id = id) m.out in
    Wrarray.for_all (fun m -> not (leads_to id m)) inst.machs
    && inst.machs.(id).attrs = [||]

let valid_output_mach inst id =
  let m = inst.machs.(id) in
    m.out = [||] && m.attrs = [||]

let write inst chan =
  let write_attrs a =
    fprintf chan "\t\t%d %d\n" a.attr_id a.proc_time; in
  let write_transports t =
    fprintf chan "\t\t%d %d\n" t.dest_id t.trans_time; in
  let write_mach m =
    fprintf chan "machine: %d\n" m.mach_id;
    fprintf chan "\tnattrs: %d\n" (Array.length m.attrs);
    Array.iter write_attrs m.attrs;
    fprintf chan "\n\tntransports: %d\n" (Array.length m.out);
    Array.iter write_transports m.out;
    fprintf chan "\n"; in
  let write_material_goal i attrs =
    fprintf chan " %d %d:" i (List.length attrs);
    List.iter (fprintf chan " %d") attrs;
    fprintf chan "\n\t" in
  let layout = inst.layout in
    fprintf chan "total number of attrs: %d\n" layout.nattrs;
    fprintf chan "total number of machs: %d\n" layout.nmachs;
    Array.iter write_mach layout.machs;
    fprintf chan "\nninputs: %d\n\t" (Array.length layout.inputs);
    Array.iter (fprintf chan " %d") layout.inputs;
    fprintf chan "\nnoutputs: %d\n\t" (Array.length layout.outputs);
    Array.iter (fprintf chan " %d") layout.outputs;
    fprintf chan "\nnmaterial: %d\n" inst.nmaterial;
    fprintf chan "\nsrcs:\n\t";
    Array.iter (fprintf chan " %d") inst.srcs;
    fprintf chan "\ndests:\n\t";
    Array.iter (fprintf chan " %d") inst.dests;
    fprintf chan "\ndesired attrs:\n\t";
    Array.iteri write_material_goal inst.desired_attrs;
    fprintf chan "\n"

let save inst fname =
  Wrio.with_outfile fname (write inst)

let read chan =
  let read_attr _ =
    let attr_id, proc_time = fscanf chan " %d %d" gather2 in
      { attr_id = attr_id; proc_time = proc_time } in
  let read_transport _ =
    let dest_id, trans_time = fscanf chan " %d %d" gather2 in
      { dest_id = dest_id; trans_time = trans_time } in
  let read_machine i =
    let id = fscanf chan " machine: %d" identity in
    let nattrs = fscanf chan " nattrs: %d" identity in
    let attrs = Array.init nattrs read_attr in
    let nout = fscanf chan " ntransports: %d" identity in
    let out = Array.init nout read_transport in
      assert (id = i);
      { mach_id = id; attrs = attrs; out = out } in
  let read_material_goal i =
    let id, nattrs = fscanf chan " %d %d:" gather2 in
    let attrs = Array.init nattrs (fun _ -> fscanf chan " %d" identity) in
      assert (id = i);
      Array.to_list attrs in
  let nattrs = fscanf chan " total number of attrs: %d" identity in
  let nmachs = fscanf chan " total number of machs: %d" identity in
  let machs = Array.init nmachs read_machine in
  let ninputs = fscanf chan " ninputs: %d" identity in
  let inputs = Array.init ninputs (fun _ -> fscanf chan " %d" identity) in
  let noutputs = fscanf chan " noutputs: %d" identity in
  let outputs = Array.init noutputs (fun _ -> fscanf chan " %d" identity) in
  let layout = { nattrs = nattrs; nmachs = nmachs; machs = machs;
                 inputs = inputs; outputs = outputs } in
  let nmaterial = fscanf chan " nmaterial: %d" identity in
  let _ = fscanf chan " srcs:" ignore in
  let srcs = Array.init nmaterial (fun _ -> fscanf chan " %d" identity) in
  let _ = fscanf chan " dests:" ignore in
  let dests = Array.init nmaterial (fun _ -> fscanf chan " %d" identity) in
  let _ = fscanf chan " desired attrs:" ignore in
  let desired_attrs = Array.init nmaterial read_material_goal in
    { layout = layout;
      nmaterial = nmaterial;
      srcs = srcs;
      dests = dests;
      desired_attrs = desired_attrs; }


let load fname =
  Wrio.with_infile fname read

(** {1 Delaunay instances} These instances are generated using a
    Delaynay triangulation of random points as was done in "Simple
    Testbed for On-line Planning," Benton, Do and Ruml, ICAPS-07.

    The graphs created by Benton et al. seem to be undirected.
    Additionally all processing takes 2 units of time and each
    transport takes 10. We would prefer directed graphs with different
    edge weights and processing times so we modify the approach
    slightly:

    Delaunay triangulation of points placed randomly on a unit square.
    All edges are directed toward the positive x direction.  Input
    machines are the left-most points on the outter hull and output
    machines are the right-most machines on the outter hull.
    Attributes are assigned randomly to each machine (excetp for input
    and output machines), processing times are assigned randomly
    within a bound and transportation costs a multiple of the
    Euclidean distance between two connected machines.
*)

(** Dumps a spt-it-out input file to the file [fname] to draw the
    layout. *)
let plot inputs outputs edges points = function
  | Some fname ->
      let fmt f = Format.fprintf Format.str_formatter f in
        fmt "(let* (@[";
        Wrlist.iteri
          (fun i (n0, n1, _) ->
             let x0, y0 = points.(n0) and x1, y1 = points.(n1) in
               fmt "(@[edge%d@ ((%g %g) (%g %g))@])@\n" i x0 y0 x1 y1;
               fmt "(@[scatter%d@ (line-points-dataset " i;
               fmt ":glyph \"circle\" :dashes () :point-radius (pt 2) ";
               fmt ":points edge%d)@])@\n" i)
          edges;
        Array.iter (fun i ->
                      fmt "(@[input%d@ (scatter-dataset @[" i;
                      fmt ":glyph \"box\"@ :point-radius (pt 10)@ ";
                      fmt ":points ((%g %g))@])@])@\n"
                        (fst points.(i)) (snd points.(i)))
        inputs;
        Array.iter (fun i ->
                      fmt "(@[output%d@ (scatter-dataset @[" i;
                      fmt ":glyph \"ring\"@ :point-radius (pt 10)@ ";
                      fmt ":points ((%g %g))@])@])@\n"
                        (fst points.(i)) (snd points.(i)))
        outputs;
        fmt "(@[plot@ (num-by-num-plot @[";
        Wrlist.iteri (fun i _ -> fmt ":dataset scatter%d@\n" i) edges;
        Array.iter (fun i -> fmt ":dataset input%d@\n" i) inputs;
        Array.iter (fun i -> fmt ":dataset output%d@\n" i) outputs;
        fmt "@])@])@\n";
        fmt "@])@\n(@[(display plot)@]))";
        Wrio.with_outfile fname
          (fun outch -> output_string outch (Format.flush_str_formatter ()))
  | None -> ()

let rand_attr nattrs min_proc max_proc =
  assert (nattrs > 0);
  assert (nattrs < (1 lsl 30));
  let ptime min max =
    if min = max then min else Wrrandom.int_in_range min max in
  { attr_id = Random.int nattrs; proc_time = ptime min_proc max_proc; }

let rand_attrs nattrs min_proc max_proc attrs_per_mach =
  assert (nattrs > 0);
  assert (nattrs < (1 lsl 30));
  let n = Random.int nattrs + 1 in
  let attrs =
    Wrutils.map_ntimes (fun () -> rand_attr nattrs min_proc max_proc) n in
  Array.of_list (Wrlist.remove_dups (fun a -> a.attr_id) attrs)

let add_attributes nattrs min_proc max_proc attrs_per_mach inputs
    outputs machs =
  let no_attrs = Hashtbl.create 257 in
  Array.iter (fun i -> Hashtbl.add no_attrs i true) inputs;
  Array.iter (fun i -> Hashtbl.add no_attrs i true) outputs;
  Array.map
    (fun m ->
      if Hashtbl.mem no_attrs m.mach_id then
        m
      else
        { m with attrs = rand_attrs nattrs min_proc max_proc attrs_per_mach })
    machs

(* @param edge_mult is multiplied by the edge weight before it is
   truncated to an integer. *)
let connect_machines mult nmachines edges inputs outputs =
  let outs = Array.create nmachines [||] in
  let consider_edge edges' ((n0, n1, wt) as e) =
    if not (Wrarray.mem n1 inputs) && not (Wrarray.mem n0 outputs)
    then begin
      let tr = { dest_id = n1; trans_time = truncate (wt *.mult) } in
        outs.(n0) <- Wrarray.extend outs.(n0) 1 tr;
        e :: edges'
    end else
      edges' in
  let edges' = List.fold_left consider_edge [] edges in
  let machs =
    Array.mapi (fun id out -> { mach_id = id; attrs = [||]; out = out; }) outs
  in
    edges', machs

(** Gets an array of the edges on the outter hull. *)
let hull_nodes edges points =
  let hull_edges = Digraph.outter_hull edges points in
  let tbl = Hashtbl.create 257 in
  let nodes = ref [] in
  let consider_node n =
    if not (Hashtbl.mem tbl n) then begin
      nodes := n :: !nodes;
      Hashtbl.add tbl n true;
    end
  in
    List.iter (fun (s, d, _) -> consider_node s; consider_node d;) hull_edges;
    Array.of_list !nodes

let inputs_and_outputs ninputs noutputs edges points =
  let min_x a b =
    let ax, _ = points.(a) and bx, _ = points.(b) in compare ax bx in
  let max_x a b =
    let ax, _ = points.(a) and bx, _ = points.(b) in compare bx ax in
  let nodes = hull_nodes edges points in
  let n = Array.length nodes in
    if ninputs + noutputs > n then
      failwith ("Not enough machines on the hull")
    else
      Array.sort min_x nodes;
    let inputs = Array.sub nodes 0 ninputs in
    let rest = Array.sub nodes ninputs (n - ninputs) in
      Array.sort max_x rest;
      let outputs = Array.sub rest 0 noutputs in
        inputs, outputs

let attribute_sets path =
  let icmp = Math.icompare in
  let module S = Set.Make(struct type t = int let compare = icmp end) in
  let progress adds s = s :: List.map (fun a -> S.add a s) adds in
  let sets =
    List.fold_left
      (fun sets adds -> Wrlist.mapcan (progress adds) sets) [S.empty] path
  in
    List.map S.elements sets

(** Merge the attributes with the table of attributes that have
    already been seen.  Returns a bool for whether or not there was a
    new attribute set. *)
let merge_attributes seen mach_id attrs =
  let sets =
    try Hashtbl.find seen mach_id with Not_found ->
      let sets = Hashtbl.create 257 in
        Hashtbl.add seen mach_id sets;
        sets in
  let consider_attrs noo attr =
    if Hashtbl.mem sets attr then
      noo
    else begin
      Hashtbl.add sets attr true;
      true
    end
  in
    List.fold_left consider_attrs false attrs

(** From the given input, compute the set of possible attribute sets
    for each other machine in the layout.  Each attribute set is a
    list, the empty attribut set is valid.  An empty set of attribute
    sets is indacitve of an un-reachable machine. *)
let reachable machines input =
  let seen = Hashtbl.create 257 in
  let rec visit path mid =
    let mach = machines.(mid) in
    let adds = Array.fold_left (fun l a -> a.attr_id :: l) [] mach.attrs in
    let path' = adds :: path in
    let sets = attribute_sets path' in
    let noo = merge_attributes seen mid sets in
      if noo then Array.iter (fun tr -> visit path' tr.dest_id) mach.out in
  let mach_attrs mid =
    try Hashtbl.fold (fun k _ l -> k :: l) (Hashtbl.find seen mid) []
    with Not_found -> []
  in
    visit [] input;
    Array.mapi (fun id _ -> mach_attrs id) machines

let reachable_outputs reachable outputs input_ind =
  Array.fold_left
    (fun outs output ->
       if reachable.(input_ind).(output) = [] then outs else output :: outs)
    [] outputs

let random_usquare_layout ?outfile ~edge_mult ~min_proc ~max_proc ~nmachs
    ~nattrs ~attrs_per_mach ~ninputs ~noutputs =
  let edges, points = Digraph.random_planar_triangles nmachs in
  let inputs, outputs = inputs_and_outputs ninputs noutputs edges points in
  let edges', machines =
    connect_machines edge_mult nmachs edges inputs outputs in
  let machines' =
    add_attributes nattrs min_proc max_proc attrs_per_mach inputs
      outputs machines in
  plot inputs outputs edges' points outfile;
  { nattrs = nattrs; nmachs = nmachs; machs = machines';
    inputs = inputs; outputs = outputs; }

let random_usquare ?outfile ?(edge_mult=10.) ?(min_proc=500) ?(max_proc=1000)
    ~nmachs ~nattrs ~attrs_per_mach ~ninputs ~noutputs ~njobs =
  let layout =
    random_usquare_layout ?outfile ~edge_mult ~min_proc ~max_proc ~nmachs
      ~attrs_per_mach ~nattrs ~ninputs ~noutputs in
  let reach = Array.map (reachable layout.machs) layout.inputs in
  let ninputs = Array.length layout.inputs in
  let reach_outs =
    Array.mapi (fun i _ -> reachable_outputs reach layout.outputs i)
      layout.inputs in
  let rand_job () =
    assert (ninputs > 0);
    assert (ninputs < (1 lsl 30));
    let input_ind = Random.int ninputs in
    let input = layout.inputs.(input_ind) in
    let outs = reach_outs.(input_ind) in
    assert (outs <> []);
    let output = List.nth outs (Random.int (List.length outs)) in
    let attr_sets = List.filter (( <> ) []) reach.(input_ind).(output) in
    assert (attr_sets <> []);
    let attrs = List.nth attr_sets (Random.int (List.length attr_sets)) in
    input, output, attrs in
  let jobs = Array.init njobs (fun _ -> rand_job ()) in
  let srcs = Array.map (fun (src, _, _) -> src) jobs in
  let dests = Array.map (fun (_, dest, _) -> dest) jobs in
  let attrs = Array.map (fun (_, _, attrs) -> attrs) jobs in
  { layout = layout;
    nmaterial = njobs;
    srcs = srcs;
    dests = dests;
    desired_attrs = attrs }

(** {1 Building sets of instances} *)

let rand_set ~nmachs ~njobs ~nattrs ~num =
  Random.self_init ();
  let edge_mult = 10. in
  let min_proc = 100 in
  let max_proc = 100 in
  let attrs_per_mach = 2 in
  let ninputs = 1 in
  let noutputs = 1 in
  let inst_root = User_paths.instance_root ^ "manufacturing_instances/" in
  let attrs = [
    "num machines", string_of_int nmachs;
    "num jobs", string_of_int njobs;
    "num attributes", string_of_int nattrs;
    "num inputs", string_of_int ninputs;
    "num outputs", string_of_int noutputs;
    "edge mult", string_of_float edge_mult;
    "min proc time", string_of_int min_proc;
    "max proc time", string_of_int max_proc;
    "max attributes per machin", string_of_int attrs_per_mach;
  ] in
  for i = 0 to num - 1 do
    let inst_attrs = attrs @ [ "num", string_of_int i ] in
    let file = Rdb.path_for inst_root inst_attrs in
    let inst =
      random_usquare ?outfile:None ~edge_mult ~min_proc ~max_proc
	~attrs_per_mach
        ~ninputs ~noutputs ~nmachs ~nattrs ~njobs
    in
    save inst file
  done
