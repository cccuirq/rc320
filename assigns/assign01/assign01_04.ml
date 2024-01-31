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


    let taxicab n =
      let rec check_a a acc = 
         let rec check_b b acc =
            if b >= n then acc
            else if (a*a*a) + (b*b*b) = n then check_b (b + 1) (acc + 1)
            else check_b (b + 1) acc
         in
            if a >= n then acc
            else check_a (a+1) (check_b a acc)
      in check_a 1 0