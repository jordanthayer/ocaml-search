
class ['node, 'saved] lds_info
    do_get_child
    do_leaf_p
    do_better_p
    do_optimal_p
    do_copy_state
    do_prune_p
    do_log
    prev_best
    halt_spec
    
    max_depth
    brancher =
object
  inherit ['node, 'saved] basic
    do_get_child
    do_leaf_p
    do_better_p
    do_optimal_p
    do_copy_state
    do_prune_p
    do_log
    prev_best
    halt_spec
  val max_depth = max_depth
  val brancher = brancher

  method get_max_depth = max_depth
  method get_brancher = brancher
end
  
