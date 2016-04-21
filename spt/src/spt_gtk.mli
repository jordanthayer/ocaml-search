(**
    @author jtd7
    @since 2010-05-06
*)

val create_display :
  < draw : Drawing.context -> 'a; height : Length.t; output : string -> unit;
    set_size : w:Length.t -> h:Length.t -> 'b; width : Length.t; .. > ->
  string -> unit

(* EOF *)
