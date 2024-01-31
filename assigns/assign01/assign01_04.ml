(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)

let taxicab (n : int) : int =
  let rec help a b n acc =
    if a > n then acc
    else if b > n then help (a + 1) (a + 1) n acc
    else
      let sum = (a * a * a) + (b * b * b) in
      let new_acc = if sum = n then acc + 1 else acc in
      help a (b + 1) n new_acc
    in help 0 0 n 0