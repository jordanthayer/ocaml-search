let outch = open_out "test_amb.results.holes.csv" in
  Test_amb.many_runs outch 20
    [ (100, 3, 1./.8.);
      (100, 3, 1./.12.);
      (100, 3, 1./.16.);
      (100, 7, 1./.16.);
      (100, 7, 1./.24.);
      (100, 7, 1./.32.)]
    [20] [1;4];
  close_out outch
