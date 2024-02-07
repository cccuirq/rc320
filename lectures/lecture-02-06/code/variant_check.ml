


(* type shape = {
  Rect of{ 
    base: Float;
    height: Float;
  }
  |Triangle of{
    side: Float * Float;
    angle: Float;
  }
} *)

  
let area (s : shape) =
  match s with
  | Rect r -> r.base *. r.height
  | Triangle { sides = (a, b) ; angle } -> Float.sin angle *. a *. b
  | Circle r -> r *. r *. Float.pi








