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
    
    (* let rec add_to_end lst x = 
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
        explore_paths start_point [] len *)

        let rec add_to_end lst x =
          match lst with
          | [] -> [x]
          | h :: t -> h :: add_to_end t x
        
        let feasible stp endp steps =
          let dist = abs(stp.x - endp.x) + abs(stp.y - endp.y) in
          dist <= steps && (steps - dist) mod 2 = 0
        
        (* let rec all_paths steps start_point end_point =
          if steps = 0 then
            if start_point = end_point then [[]] else []
          else if not (feasible start_point end_point steps) then
            []
          else
            let directions = [N; S; E; W] in
            let rec explore_paths current_point path_remaining steps =
              if steps = 0 then
                if current_point = end_point then [List.rev path_remaining] else []
              else
                List.concat (List.map (fun dir ->
                  let next_point = move current_point dir in
                  if feasible next_point end_point (steps - 1) then
                    let next_step = (dir, 1) in
                    explore_paths next_point (next_step :: path_remaining) (steps - 1)
                  else
                    []
                ) directions)
            in
            explore_paths start_point [] steps *)

let rec explore_paths current_point end_point path_remaining steps =
  if steps = 0 then
    if current_point = end_point then [path_remaining] else []
  else
    let directions = [N; S; E; W] in
    explore_and_concat directions current_point end_point path_remaining steps []

and explore_and_concat directions current_point end_point path_remaining steps acc =
  match directions with
  | [] -> acc
  | dir :: rest ->
    let next_point = move current_point dir in
    if feasible next_point end_point (steps - 1) then
      let next_step = (dir, 1) in
      let new_paths = explore_paths next_point end_point (add_to_end path_remaining next_step) (steps - 1) in
      explore_and_concat rest current_point end_point path_remaining steps (acc @ new_paths)
    else
      explore_and_concat rest current_point end_point path_remaining steps acc

let all_paths steps start_point end_point =
  if steps = 0 then
    if start_point = end_point then [[]] else []
  else if not (feasible start_point end_point steps) then
    []
  else
    explore_paths start_point end_point [] steps