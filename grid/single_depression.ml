(**

    @author jtd7
    @since 2012-07-23
   Builds an instance with a huge C between the start and the goal
*)

let make inum h w =
  let blocked = Array.create_matrix h w false
  and goalx = w - 1
  and goaly = h / 2
  and startx = 0
  and starty = h / 2 in
  for i = 1 to w - 2 do
    (blocked.(i).(1) <- true;
     blocked.(i).(h-2) <- true;)
  done;
  for i = 1 to h - 2 do
    blocked.(w-2).(i) <- true
  done;
  { Grid.blocked = blocked;
    costs = Grid.Unit;
    moves = Grid.Fourway;
    goal = [goalx,goaly];
    start = startx,starty;
    instance = inum;}

let make_default_cup ?(inum = -1) height width =
  make inum height width

let save = Grid_instance.save
