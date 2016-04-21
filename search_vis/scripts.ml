(** Some basic scripts for to use on the search vis *)


let wrap_firsts disp =
  Display.set_getters disp
    (Array.map (Render.wrap_first_in_struct
		  (Array.of_list (Display.get_structs disp)))
       disp.Display.getters)

let highlight_list disp list color =
Display.set_getters disp
    (Array.map (Render.wrap_mem ~colors:[|color|]
       [|List.nth (Display.get_structs disp) list|]) disp.Display.getters)



(* EOF *)
