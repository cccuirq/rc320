type day = M | T | W | Th | F | S | Su

let after_day (d : day) (i : int) : day =
  let calc_index d i = 
    match d with
    |M -> i
    |T -> i+1
    |W -> i+2
    |Th -> i+3
    |F -> i+4
    |S -> i+5
    |Su -> i+6
    |_ -> assert false
  in let check d =
    match d with
    |0 -> M
    |1 -> T
    |2 -> W
    |3 -> Th
    |4 -> F
    |5 -> S
    |6 -> Su
    |_ -> assert false
  in check ((calc_index d i ) mod 7)


let _ = assert (after_day W 17 = F)
let _ = assert (after_day W (-1) = T)

type point = { x : float ; y : float }
type p_coord = { d : float ; angle : float }
let to_polar (p : point) = (* TODO *)
  {d = sqrt(p.x *. p.x +. p.y *. p.y);
  angle = atan(p.y /. p.x);}