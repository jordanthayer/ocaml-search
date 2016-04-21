open Grid
open Grid_algs

let richter_l1 w lim =
  with_path6 (Restarts_wastar.dups (default_interface w lim)
		Restarts_wastar.richterl1)

let richter_l1_dd w lim =
  with_path6 (Restarts_wastar.delay_dups (default_interface w lim)
		Restarts_wastar.richterl1)

let richter_l2 w lim =
  with_path6 (Restarts_wastar.dups (default_interface w lim)
		Restarts_wastar.richterl2)

let richter_l2_dd w lim =
  with_path6 (Restarts_wastar.delay_dups (default_interface w lim)
		Restarts_wastar.richterl2)

let ara_l1 w lim =
  with_path6 (Arastar.dups (default_interface w lim) Restarts_wastar.richterl1)

let ara_l2 w lim =
  with_path6 (Arastar.dups (default_interface w lim) Restarts_wastar.richterl2)

(* EOF *)
