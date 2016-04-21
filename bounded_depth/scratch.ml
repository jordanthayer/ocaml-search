
type foo = {
  a : int;
  f : int -> bool;
}

let g x y =
  (x.a + y) > 5

let make_foo i =
  let rec foo = { a = i;
		  f = g foo; } in
    foo
