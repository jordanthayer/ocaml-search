(**

    @author sna4
    @since 2010-01-05
*)

let dta_dups sface args =
  Dtastar_old.dups sface args

let dta_no_dups sface args =
  Dtastar_old.no_dups sface args

let dta_learn sface args =
  Dtastar_old.dta_learn sface args

let rta_dups sface args =
  Rtastar.dups sface args

let rta_no_dups sface args =
  Rtastar.no_dups sface args
