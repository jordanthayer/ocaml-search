(** Left Leaning Red Black Tree following Sedgewick *)

let red = true
and black = false

exception Not_found

type 'a node = {
  mutable data : 'a;
  mutable left : 'a node; (* less than 'a *)
  mutable right : 'a node; (* at least 'a *)
  mutable parent : 'a node;
  mutable color : bool
}

type 'a t = {
  mutable root : 'a node;
  less_than : 'a -> 'a -> bool;
  equals : 'a -> 'a -> bool;
  nil : 'a node; (* dummy sentinel value *)
  mutable count : int;
}


let make_with ?(equals = (fun a b -> a = b )) predicate dummy_val =
  let rec nil = { data = dummy_val;
		  left = nil;
		  right = nil;
		  parent = nil;
		  color = black; } in
    { root = nil;
      less_than = predicate;
      nil = nil;
      equals = equals;
      count = 0; }


let count t = t.count


let rotateLeft node =
  let x = node.right in
    node.right <- x.left;
    node.right.parent <- node;
    x.left <- node;
    node.parent <- x;
    x.color <- x.left.color;
    x.left.color <- red;
    x


let rotateRight node =
  let x = node.left in
    node.left <- x.right;
    node.left.parent <- node;
    x.right <- node;
    node.parent <- x;
    x.color <- x.right.color;
    x.right.color <- red;
    x


let flipColors node =
  node.color <- not node.color;
  node.left.color <- not node.left.color;
  node.right.color <- not node.right.color


let search t key =
  let rec search root =
    if root == t.nil then raise Not_found;
    if t.equals key root.data
    then root
    else (if not (t.less_than root.data key)
	  then search root.left
	  else search root.right) in
    search t.root


let isRed t node =
  node != t.nil && node.color == red


let fixup t node =
  let h = ref node in
    if isRed t !h.right
    then h := rotateLeft !h;
    if isRed t !h.left &&  isRed t !h.left.left
    then h := rotateRight !h;
    if isRed t !h.left && isRed t !h.right
    then flipColors !h;
    !h


let rec min t node =
  if node.left == t.nil
  then node
  else min t node.left

let rec max t node =
  if node.right == t.nil
  then node
  else max t node.right


let rec do_insert t ins_node key = (* follows private insert *)
  if ins_node == t.nil
  then  { data = key;
	  left = t.nil;
	  right = t.nil;
	  parent = t.nil;
	  color = red}
  else
    (if (isRed t ins_node.left) && (isRed t ins_node.right)
     then flipColors ins_node;
     (if t.less_than key ins_node.data
      then (ins_node.left <- do_insert t ins_node.left key;
	    ins_node.left.parent <- ins_node)
      else (ins_node.right <- do_insert t ins_node.right key;
	    ins_node.right.parent <- ins_node));
     (if (isRed t ins_node.right) && not (isRed t ins_node.left)
      then (rotateLeft ins_node)
      else (if (isRed t ins_node.left) && (isRed t ins_node.left.left)
	    then (rotateRight ins_node)
	    else ins_node)))


let insert t key = (* follows public insert *)
  t.count <- t.count + 1;
  t.root <- do_insert t t.root key;
  t.root.color <- black


let moveRedLeft t h =
  flipColors h;
  if isRed t h.right.left
  then (h.right <- rotateRight h.right;
	h.right.parent <- h;
	let to_ret = rotateLeft h in
	  flipColors to_ret;
	  to_ret)
  else h


let moveRedRight t h =
  flipColors h;
  if isRed t h.left.left
  then (let to_ret = rotateRight h in
	  flipColors to_ret;
	  to_ret)
  else h


let rec doDeleteMin t h =
  if h.left == t.nil then t.nil
  else (let m = (if not (isRed t h.left) && not (isRed t h.left.left)
		 then moveRedLeft t h
		 else h) in
	  m.left <- doDeleteMin t m.left;
	  m.left.parent <- m;
	  fixup t m)


let deleteMin t =
  t.count <- t.count - 1;
  t.root <- doDeleteMin t t.root;
  t.root.color <- black


let rec doDelete t h key =
  Wrutils.pr "n: %i lc: %i rc: %i k: %i\n" h.data h.left.data h.right.data key;
  if not (t.less_than h.data key)
  then (let to_ret = if not (isRed t h.left) && not (isRed t h.left.left)
	then moveRedLeft t h else h  in
	  Wrutils.pr "Case 1\n";
	  to_ret.left <- doDelete t to_ret.left key;
	  to_ret.left.parent <- to_ret;
	  fixup t to_ret)
  else
    (let h = ref h in
       (if isRed t !h.left
	then (if t.equals !h.data !h.left.data && !h.left != t.nil && !h.right != t.nil
	      then Wrutils.pr "Dangerous case\n";
	      h := rotateRight !h));
       if t.equals !h.data key && !h.right == t.nil
       then (Wrutils.pr "Case 2\n"; t.nil)
       else
	 ((if not (isRed t !h.right) && not (isRed t !h.right.left)
	   then (Wrutils.pr "Moving right!\t"; h := moveRedRight t !h));
	  (* right child may be as large as current node *)
	  (if t.equals !h.data key
	   then (Wrutils.pr "Case 4\n";
		 !h.data <- (min t !h.right).data;
		 !h.right <- doDeleteMin t !h.right;
		 !h.right.parent <- !h;)
	  else (Wrutils.pr "Case 5\n";
		!h.right <- doDelete t !h.right key;
		!h.right.parent <- !h));
	  fixup t !h))


let delete t key =
  t.count <- t.count - 1;
  t.root <- doDelete t t.root key;
  t.root.color <- black



(*************** consistency checks ************)


let check_links t =
  let rec check n =
    if n == t.nil then (* nil's parent and children are arbitrary *)
      ()
    else if not ((t.equals n.data t.root.data) || (* we're the root *)
		   (t.equals n.parent.left.data n.data) || (* lc *)
		   (t.equals n.parent.right.data n.data))  (* rc *)
    then failwith "Llrbt.check_links: child not a child of its parent"
    else
      (if (n.left != t.nil) then
	 if (not (t.equals n.left.parent.data n.data)) then
	   failwith (Wrutils.str
		       "Llrbt.check_links: left child %i doesn't point to parent %i"
		       n.left.data n.data)
	 else
	   check n.left;
       if (n.right != t.nil) then
	 if (not (t.equals n.right.parent.data n.data)) then
	   failwith "Llrbt.check_links: right child doesn't point to parent"
	 else
	   check n.right)
  in
    check t.root


let check_order t =
  let rec check_children n =
    if n != t.nil then
      (if (n.left != t.nil) then
	 if not (t.less_than n.left.data n.data) then
	   failwith (Wrutils.str "Llrbt.check_order: left % i >= node %i"
		       n.left.data n.data)
	 else
	   check_children n.left;
       if (n.right != t.nil) then
	 if not (t.less_than n.data n.right.data) then
	   failwith (Wrutils.str "Llrbt.check_order: node %i > right %i"
		       n.data n.right.data)
	 else
	   check_children n.right)
  in
    check_children t.root


let check_color t =
  (** root is black, no two red nodes in a row, dummy node is black *)
  if t.root.color == red then failwith "Llrbt.check_color: root is red";
  if t.nil.color == red then failwith "Llrbt.check_color: dummy node is red";
  let rec check_children n =
    if n == t.nil then
      ()
    else
      (if n.color == red then
	 (if n.left.color == red then
	    failwith (Wrutils.str
			"Llrbt.check_color: red node %i with red left child %i"
			n.data n.left.data);
	  if n.right.color == red then
	    failwith (Wrutils.str
			"Llrbt.check_color: red node %i with red right child %i"
		     n.data n.right.data));
       check_children n.left;
       check_children n.right)
  in
    check_children t.root


let check_height t =
  (** all leaves should have same black depth *)
  let max = ref None in
  let rec descend n depth =
    if n == t.nil then
      (match !max with
	 None -> max := Some depth
       | Some d ->
	   if d != depth then
	     failwith
	       (Wrutils.str
		  "Llrbt.check_height: depth %d != expected %d" depth d))
    else
      let depth = if n.color == red then depth else depth + 1 in
	descend n.left depth;
	descend n.right depth
  in
    descend t.root 0


let count_nodes t =
  let nil = t.nil in
  let rec visit n =
    if n == nil then
      0
    else
      1 + (visit n.left) + (visit n.right)
  in
    visit t.root


let check_count t n =
  let n2 = count t
  and nc = count_nodes t in
    if (n2 <> nc) then
      failwith (Wrutils.str "Llrbt.check_count: count not right %d vs %d" n2 nc);
    if n2 <> n then
      failwith (Wrutils.str "Llrbt.check_count got %d nodes instead of %n" n2 n)


let check t n =
  check_links t;
  check_order t;
  check_color t;
  check_height t;
  check_count t n



(************** debugging benchmarks ***********)


let print_debug_using t =
  let indent d =
    Wrutils.pr "%s" (String.make (d * 3) ' ') in
  let rec print n d =
    if n == t.nil then
      ()
    else
      (print n.left (d+1);
       indent d;
       Wrutils.pr "%d (p=%d)" n.data n.parent.data;
       if n.color == red then Wrutils.pr " *red";
       Wrutils.pr "\n";
       print n.right (d+1))
  in
    Wrutils.pr "nil's parent is %d\n" t.nil.parent.data;
    print t.root 0



let print_using ch func t =
  let rec print n d =
    if n == t.nil then
      ()
    else
      (print n.left (d+1);
       output_string ch (String.make (d * 3) ' ');
       func ch n.data;
       if n.color == red then output_char ch '*';
       output_char ch '\n';
       print n.right (d+1))
  in
    print t.root 0



let print_int_tree t =
  Wrutils.pr "Result:\n";
  (* print_debug_using t; *)
  print_using stdout Wrio.output_int t;
  Wrutils.pr "-------\n"


let next t n =
  (** node ordered immediately after [n] in [t] or t.nil if none exists. *)
  let nil = t.nil in
    if n.right != nil then
      (* look down *)
      min t n.right
    else
      (* must go up *)
      let rec ascend n parent =
	if ((parent != nil) && (t.equals n.data parent.right.data)) then
	  ascend parent parent.parent
	else
	  (* if parent == nil, then came to root from the right, so no more
	     nodes and we should return nil (== parent).  if n !=
	     parent.right, we came to parent from left, so parent itself is
	     the next node. *)
	  parent
      in
	ascend n n.parent


let random t =
  (** returns a random node *)
  let max = count t in
    if max = 0 then
      failwith "Llrbt.random: can't take random node from empty tree"
    else
      let n = ref (min t t.root) in
	Wrutils.ntimes (fun () ->
			  n := next t !n)
	  (Random.int max);
	!n



let add_random t m v =
  let e = Random.int m
  and n = count t in
    if v then Wrutils.pr "Inserting %d.\n" e;
    insert t e;
    if v then print_int_tree t;
    check t (n+1)


let delete_random t v =
  let n = count t
  and node = random t in
    if v then Wrutils.pr "Deleting %d, parent %d, lc %d, rc %d.\n"
      node.data node.parent.data node.left.data node.right.data;
    delete t node.data;
    if v then print_int_tree t;
    check t (n-1)


let test1 n m v =
  let t = make_with (<=) (-1) in
    Wrutils.ntimes (fun () -> add_random t m v)
      n;
    Wrutils.pr "\n\nInserting Complete, starting deletion tests\n\n%!";
    Wrutils.ntimes (fun () -> delete_random t v)
      n;

(* EOF *)
