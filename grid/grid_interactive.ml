(* Test harness for interactive visualitzations*)

open Grid_algs
open Grid

let dHeight = 1000
and dWidth = 1000


let make_interactive_expand w display =
  let expand = make_expand w in
    (fun n g -> Display.actionloop display;
       expand n g)



(* EOF *)
