(**
    @author jtd7
    @since 2010-05-06
*)

val save :
  ?width:Length.t ->
  ?height:Length.t ->
  < draw : Drawing.context -> 'a; height : Length.t;
    set_size : w:Length.t -> h:Length.t -> 'b; width : Length.t; .. > ->
  string -> unit

(* EOF *)
