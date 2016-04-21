(**

    @author jtd7
    @since 2012-09-30
   Bootstrapping runs for the sliding tiles problem

   just gonig to hardcode 5x5 because I am in a hurry and also a bad person
*)

let make_instance instance_index =
  let name = (Printf.sprintf "boot-instance-%i" instance_index)
  and instance = Sliding_tiles.rwalk ~nrows:5 ~ncols:5 1000 in
  name,instance

let load_instance = Sliding_tiles_inst.load

let base_sface = Sliding_tiles_interfaces.default_interface "unit"

let domain = Search_interface.Tiles


(* args order:
   ins_min: Minimum number of instances for bootstrapping
   t_min: Minimum per instance timeout
   t_max: Maximum per instance timeout
*)

let astar_run args =
  Greedy_bootstrap.do_dups
    ~make_instance
    ~load_instance
    ~base_sface
    ~domain
    ~args


let greedy_run args =
  Greedy_bootstrap.do_dups
    ~make_instance
    ~load_instance
    ~base_sface
    ~domain
    ~args


let greedier_run args =
  Greedy_bootstrap.do_dd
    ~make_instance
    ~load_instance
    ~base_sface
    ~domain
    ~args


