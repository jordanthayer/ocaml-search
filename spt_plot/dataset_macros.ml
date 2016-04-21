(**

    @author jtd7
    @since 2010-05-28

*)

(* Some tools for filtering out unsolved instances *)

let filter_unsolved_instances dset =
  Dataset.filter Fn.identity (fun e -> e = "yes") "found solution" dset


let filter_solved_instances dset =
  Dataset.filter Fn.identity (fun e -> e = "no") "found solution" dset


let get_unsolved_instances ?(ikey = "num") ?(itrans = int_of_string) dset =
  let usi = filter_solved_instances dset in
    Array.to_list (Dataset.get_values itrans ikey usi)


let get_solved_instances ?(ikey = "num") ?(itrans = int_of_string) dset =
  let si = filter_unsolved_instances dset in
    Array.to_list (Dataset.get_values itrans ikey si)


let all_unsolved_instances ?(ikey = "num") ?(itrans = int_of_string) sets =
  List.fold_left (fun accum set ->
		    accum @ (get_unsolved_instances ~ikey ~itrans set))
    [] sets


let filter_instance_list ikey ilist set =
  Dataset.filter int_of_string (fun e -> not (List.mem e ilist)) ikey set


let filter_all_unsolved ?(ikey = "num") ?(itrans = int_of_string) sets =
  let unsolved = all_unsolved_instances ~ikey ~itrans sets in
    List.map (filter_instance_list ikey unsolved) sets


let solved_count dset =
  Dataset.size (filter_unsolved_instances dset)

let unsolved_count dset =
  (Dataset.size dset) - (solved_count dset)


let test_dset () =
  (Jtd7_helpers.load_wrap
     (Load_dataset.random_tanker 10 30 10 50 10 10 0.7))
    ("dfs", "dfs", [])

(* EOF *)
