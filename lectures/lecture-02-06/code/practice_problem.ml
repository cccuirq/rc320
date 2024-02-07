(* Practice problem for lecture *)

let downup (n : int) : int list = 
  let rec help n =
    match n with
    |0 -> []
    |1 -> [1]
    |_ -> n :: (help (n-1) @ [n])
  in help n