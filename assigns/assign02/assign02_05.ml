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
    let move point dir =
      match dir with
      | N -> { point with y = point.y + 1 }
      | S -> { point with y = point.y - 1 }
      | E -> { point with x = point.x + 1 }
      | W -> { point with x = point.x - 1 }
    
    let rec add_to_end lst x = 
      match lst with
      | [] -> [x]
      | h::t -> h :: add_to_end t x
    
    let rec all_paths len start_point end_point =
      if len = 0 then
        if start_point = end_point then [[]] else []
      else if len = 1 then
        []
      else
        let directions = [N; S; E; W] in
        let rec explore_paths current_p remaining len =
          if len = 0 then
            if current_p = end_point then [remaining] else []
          else
            explore_directions directions current_p remaining len
        and explore_directions dirs current_point remaining len =
          match dirs with
          | [] -> []
          | dir::rest ->
            let next_point = move current_point dir in
            let next_path = (dir, 1) :: remaining in
            let next_paths = explore_paths next_point next_path (len - 1) in
            next_paths @ explore_directions rest current_point remaining len
        in
        explore_paths start_point [] len