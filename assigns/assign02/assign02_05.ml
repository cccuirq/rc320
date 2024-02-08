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

let move (direction : dir) (point : point) (steps : int) : point =
  match direction with
  | N -> { point with y = point.y + steps }
  | S -> { point with y = point.y - steps }
  | E -> { point with x = point.x + steps }
  | W -> { point with x = point.x - steps }

let equal_points p1 p2 = p1.x = p2.x && p1.y = p2.y

let opposite direction =
  match direction with
  | N -> S
  | S -> N
  | E -> W
  | W -> E

let rec all_paths len stp endp =
  let rec explore_path len stp acc last_dir =
    if len = 0 then
      if equal_points stp endp then [List.rev acc] else []
    else
      let directions = [N; S; E; W] in
      let rec add_paths dirs acc =
        match dirs with
        | [] -> acc
        | dir :: ds ->
          if Some dir = last_dir then add_paths ds acc
          else
            let new_point = move dir stp 1 in
            let paths = explore_path (len - 1) new_point ((dir, 1) :: acc) (Some (opposite dir)) in
            add_paths ds (acc @ paths)
      in add_paths directions []
  in
  explore_path len stp [] None
    explore_path len stp [] None