(**

    @author jtd7
    @since 2012-04-18
*)

let alt_col_name = "d_histo"

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["bucket"; "count";]


let output_row ~bucket ~count =
  (Datafile.write_alt_row_prefix stdout alt_col_name;
   Verb.pr Verb.always "%i\t%i\n" bucket count)
