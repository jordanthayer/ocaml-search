(* Stored version of a run *)

(* construct the display window *)
let run_window = ignore(GtkMain.Main.init());
  GWindow.window ~border_width:10 ~title:"Run Data"
    ~deletable:false ~width:200 ~height:200 ()

(* construct the container widget *)
let val_table = GPack.table ~columns:2 ~rows:3 ~row_spacings:10
  ~col_spacings:50 ~packing:run_window#add ()


(* add all of the things into the container widget *)
let data_lbls =
  ignore(GMisc.label ~text:"Step"
	   ~packing:(val_table#attach ~left:0 ~top:0) ());
  ignore(GMisc.label ~text:"Expanded" ~packing:(val_table#attach ~left:0 ~top:1) ());
  ignore(GMisc.label ~text:"Generated" ~packing:(val_table#attach ~left:0 ~top:2) ());
  [|GMisc.label ~text:"" ~packing:(val_table#attach ~left:1 ~top:0) ();
    GMisc.label ~text:"" ~packing:(val_table#attach ~left:1 ~top:1) ();
    GMisc.label ~text:"" ~packing:(val_table#attach ~left:1 ~top:2) ();|]


type 'a problem =
  | GridProblem of 'a Grid_display.t

type 'a t = {
  run : 'a Recorded_run.t;
  problem : 'a problem;
  mutable mins : float array;
  mutable maxs : float array;
  run_leng : int;
}

(****************************************** Structs ********************)

let set_mins_maxs t getters =
  Recorded_run.iter
    (fun n ->
       Array.iteri
	 (fun get_ind getter ->
	    t.mins.(get_ind) <- min t.mins.(get_ind)
	      (match (getter n ~-1) with Render.Scaler v -> v | _ -> infinity);
	    t.maxs.(get_ind) <- max t.mins.(get_ind)
	      (match (getter n ~-1) with Render.Scaler v -> v | _ -> -.infinity))
	 getters) t.run


let reset_getters t getters =
  t.mins <- Array.create (Array.length getters) infinity;
  t.maxs <- Array.create (Array.length getters) (-.infinity);
  set_mins_maxs t getters



let load_grid_run ?(tr_file = "") ex_file str_list p_file =
  (** Loads a recorded grid run
      [tr_file] True heuristics file
      [ex_file] records node expansions of the search
      [str_list] list of data structures associated with the data file
      [p_file] the problem file *)
  let r = Run_reader.grid_run ~tr_file:tr_file ex_file str_list in
    Verb.pe Verb.always "Loded Run\n";
  let t =
    { run = r;
      problem = GridProblem (Grid_display.load p_file);
      mins = [||];
      maxs = [||];
      run_leng = Array.length r.Recorded_run.sequence;}
  in
    Verb.pe Verb.always "struct build\n";
    t


let load_tile_run tr_file ex_file str_list p_file =
  failwith "not implemented"

let load_tsp_run tr_file ex_file str_list p_file =
  failwith "not implemented"

let load_robot_run tr_file ex_file str_list p_file =
  failwith "not implemented"


let draw_info run iter =
  (** Draws the run information starting at location [sx] [sy] *)
  data_lbls.(0)#set_text (Wrutils.str "%i of %i" iter run.run_leng);
  data_lbls.(1)#set_text (Wrutils.str "%i"
			    (Array.length run.run.Recorded_run.sequence));
  data_lbls.(2)#set_text (Wrutils.str "%i"
			    (Hashtbl.length run.run.Recorded_run.run))


(* EOF *)
