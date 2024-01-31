(* Pythagorean Triples

   Implement the function `py_trip_hyp` of type `int -> bool` which
   given

   n : nonnegative integer

   returns `true` if `n` can be the hypotenuse of a right triangle
   with integer side lengths (i.e., there are `a` and `b` such that `a
   * a + b * b = n * n` is `true`).

   Examples:
   let _ = assert (py_trip_hyp 5)
   let _ = assert (py_trip_hyp 13)
   let _ = assert (py_trip_hyp 17)
   let _ = assert (py_trip_hyp 29)
   let _ = assert (not (py_trip_hyp 28))
   let _ = assert (not (py_trip_hyp 6))

*)

let py_trip_hyp n =
   let rec help a b n bo =
      if a > n then false
      else if b > n then help (a+1) (a+1) n bo
      else
         let sum = a * a + b * b in
         let bo = (sum = n) in help a (b+1) n bo
      in help 0 0 n false

let py_trip_hyp_demo n =
   let rec check_a(a:int):bool = 
      let rec check_b b =
         if b >= n then false
         else if((a*a) + (b*b)) = (n*n) then true
         else check_b(b + 1)
      in
         if a >= n then false
         else if check_b a = true then true
         else check_a (a+1)
   in check_a 1