(*

  little module for choosing actions.  Extremely basic value
  iteration.  Useful for solving a K-arm bandit problem, used here to
  select restart predicates for a beam search.  

  Randomly selects actions for the first k*5 actions, then selects
  randomly with chance 1/n otherwise selects greedily.

*)

type action_chooser = 
    { num_actions : int;
      mutable random_tries : int;
      mutable total_calls : float;
      action_counts : float array;
      action_results : float array;
      action_average : float array;
      mutable best_action : int; }

let create k = 
  {num_actions = k;
   random_tries = 0;
   total_calls = 0.;
   action_counts = Array.make k 0.0;
   action_results = Array.make k 0.0;
   action_average = Array.make k 0.0;
   best_action = 0; }


let choose_action ac = 
  if(ac.random_tries < ac.num_actions * 5) then
    (ac.random_tries <- ac.random_tries + 1;(**)
     (Math.random_int ()) mod ac.num_actions)
  else if (Math.true_with_prob (1. /. ac.total_calls)) then 
    (ac.random_tries <- ac.random_tries + 1;
     (Math.random_int ()) mod ac.num_actions)
  else 
    ac.best_action

let register_result ac id value = 
  assert (id < ac.num_actions);
  assert (id >= 0);
  ac.total_calls <- ac.total_calls +. 1.;
  ac.action_counts.(id) <-  ac.action_counts.(id) +. 1.0;
  ac.action_results.(id) <-  ac.action_results.(id) +. value;
  ac.action_average.(id) <- ac.action_results.(id) /. ac.action_counts.(id);
  ac.best_action <- Wrarray.max_index_by (fun n -> n) ac.action_results
