(* Icy Hot

   Implement the function `reduce` which given

     l : a list of `temp`s

   returns a new list of `temp`s gotten by the following reduction rule:

   If `Hot i` and `Icy i` are adjacent (in particular, they must be
   carrying the same value) in any order, then they cancel out and are
   removed from the list.

   This rule should be carried out until it not possible to reduce the
   list any further.

   Examples:
   let _ = assert (reduce [Hot 0;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Icy 1;Hot 1;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Hot 1;Icy 2;Hot 2;Hot 0;Icy 1] = [Hot 0;Hot 1;Hot 0;Icy 1])

*)

type temp
  = Hot of int
  | Icy of int

let reduce (l : temp list) : temp list =
  let rec loop_list (list : temp list) : temp list =
    match list with
    | [] -> list
    | [head] -> list 
    | Hot h::Icy i::tail -> if h = i then loop_list tail else Hot h::loop_list (Icy i::tail) 
    | Icy h::Hot i::tail -> if h = i then loop_list tail else Icy h::loop_list (Hot i::tail) 
    | h::i::tail -> h::loop_list (i::tail) 
  in 
  let rec loop_again list =
    let news = loop_list list
    in 
    if list = news then news
    else loop_again news
  in 
  loop_again l

