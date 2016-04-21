(**

   code for asserts and error generation.

*)

let assert_with_message (test:bool) (message:string) = 
  if(test) then ()
  else 
    failwith message;
