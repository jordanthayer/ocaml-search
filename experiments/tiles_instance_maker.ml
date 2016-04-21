(** Useful for generating random instances of the sliding tile puzzle *)

let instance_root = Experiments.instance_root ^ "tiles_instances"

let make_random_instances overwrite nrows ncols count =
  (* this isn't guaranteed to m ake solvable instances *)
  Sliding_tiles_inst.make_random_set ~overwrite ~nrows ~ncols ~count


let make_rwalk_instances overwrite nrows ncols count =
  (* this is *)
  Sliding_tiles.make_rwalk_set ~overwrite ~nrows ~ncols ~count


let scale_study ?(count = 100) min max =
  for i = min to max do
    make_rwalk_instances false i i count
  done
(* EOF *)
