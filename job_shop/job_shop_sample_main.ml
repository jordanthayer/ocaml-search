(**

   @author eaburns
   @since 2010-03-01
*)


let inst = Job_shop_instance.load
  "./group/data/job_shop/random/5/5/1";;

let ub = Job_shop_instance.make_span_ub inst ;;

Verb.set_default Verb.debug;;

Verb.pr Verb.toplvl "ub=%d\n%!" ub;;

let prob = { Job_shop.deadline = ub;
	     Job_shop.instance = inst;
	     Job_shop.best_incumbent = max_int;
	   };;

(*
  Job_shop_sample.learn_model prob 1000;;
*)
Job_shop_sample.enumerate_leaves prob;;
