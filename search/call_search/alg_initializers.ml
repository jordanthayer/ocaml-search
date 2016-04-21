(** An algorithm initialization tool
    Jordan - July 2009 *)


let string_array_init
    alg
    solution_handler
    interface_builder
    (args : string array) =
  (fun problem limit ->
     let iface = interface_builder problem limit
     and args = (match (Array.to_list args) with
			  | [] -> [||] (* strip off alg name *)
			  | hd::tl -> Array.of_list tl) in
       (fun () -> solution_handler (alg iface args)))

(*EOF*)
