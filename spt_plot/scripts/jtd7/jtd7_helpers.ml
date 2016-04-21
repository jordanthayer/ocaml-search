(**

    @author jtd7
    @since 2010-05-28

   A collection of helper functions  for me
*)


let load_wrap ?(o_attrs = []) fn (id,name,attrs) =
  fn (("alg", id)::(o_attrs@attrs)) name


(* EOF *)
