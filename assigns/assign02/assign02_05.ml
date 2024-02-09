(* Listing Paths

   Implement a function `all_paths` which given

     len : a nonnegative integer
     stp : a point (see below)
     endp : a point

   returns a list of `(dir * int)` lists, where each list contains
   sequence of directions to get from `stp` to `endp` in exactly `len`
   steps.

   Notes:
   A sequence of directions is a list of `dir * int` pairs with the
   property that no adjacent elements have the same `dir`.  For example,

     [(N, 1); (S, 1); (N, 3)]

   is a valid sequence of directions but

     [(N, 1); (N, 3); (S, 1)]

   is not. Instead, this should be

     [(N, 4), (S, 1)]

   Examples:
   let origin = {x=0;y=0}
   let _ = assert (all_paths 0 origin origin = [[]])
   let _ = assert (all_paths 1 origin origin = [])
   let _ = assert (all_paths 2 origin origin =
       [[(N,1);(S,1)] ;
        [(S,1);(N,1)] ;
        [(E,1);(W,1)] ;
        [(W,1);(E,1)] ])
   let _ = assert (all_paths 3 origin origin = [])
   let _ = assert (List.mem [(N,2);(S,2)] (all_paths 4 origin origin))
   let _ = assert (List.mem [(N,1);(E,1);(W,1);(S,1)] (all_paths 4 origin origin))

*)

type dir = N | S | E | W

type point = {
  x : int ;
  y : int ;
}
  let rec check_valid path =
    match path with
    | (dir1, dis1) :: (dir2, dis2) :: rest when dir1 = dir2 -> check_valid ((dir1, dis1 + dis2) :: rest)  (* Combine consecutive moves in the same direction *)
    | head :: rest -> head :: check_valid rest  (* Keep the head and process the rest *)
    | [] -> []

let rec add_to_end paths = 
  match paths with
  | [] -> []
  | h::t -> check_valid h :: add_to_end t

  let rec list_each_add item l =
    match l with 
    | [] -> []
    | [] :: rest -> list_each_add item rest
    | path :: rest ->
      let new_current =  item :: path in
      new_current :: (list_each_add item rest )

  let rec all_paths len start dst =
    let only_one start dst = 
      if start.x - dst.x = 1 && start.y = dst.y then [[(W, 1)]]
      else if dst.x - start.x = 1 && start.y = dst.y then [[(E, 1)]]
      else if start.y - dst.y = 1 && start.x = dst.x then [[(S, 1)]]
      else if dst.y - start.y = 1 && start.x = dst.x then [[(N, 1)]]
      else []
    in
    if len = 0 then
      [[]]
    else if len = 1 then
      only_one start dst
    else
      let n_paths = add_to_end (list_each_add (N, 1) (all_paths (len-1) {x = start.x; y = start.y+1} dst)) in
      let s_paths = add_to_end (list_each_add (S, 1) (all_paths (len-1) {x = start.x; y = start.y-1} dst)) in
      let w_paths = add_to_end (list_each_add (W, 1) (all_paths (len-1) {x = start.x-1; y = start.y} dst)) in
      let e_paths = add_to_end (list_each_add (E, 1) (all_paths (len-1) {x = start.x+1; y = start.y} dst)) in
      n_paths @ s_paths @ w_paths @ e_paths