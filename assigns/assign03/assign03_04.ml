(* Matrices

   In this problem you will be building a (very) small interface for
   matrices.  A matrix is represented as a record which keeps track of
   its numbers of rows and columns as well as the values represented
   as a list of rows (i.e., a list of lists).  You will be
   implementing some error handling by working with `result`s.

   ************

   Task 1: Implement a function `mkMatrix` which given

     rs : a list of lists

   returns a matrix represented by this list of lists if it is valid
   or an `error` otherwise.  The error conditions are:

   * If the lengths of the rows in `rs` are not all equal then
   `mkMatrix` should return `Error UnevenRows`.

   * If `rs` is empty then `mkMatrix` should return `Error ZeroRows`.

   * If `rs` contains only empty lists, then `mkMatrix` should reutrn
   `Error ZeroCols`.

   In other words, `mkMatrix` should only return a matrix if `rs`
   represents a nonempty rectangular grid of values.

   Example:
   let l = [[1;2;3];[4;5;6]]
   let rm = mkMatrix l
   let _ = match rm with
     | Ok m ->
       let _ = assert (m.num_rows = 2) in
       let _ = assert (m.num_cols = 3) in
       ()
     | _ -> assert false

   let r = [[1;2;3;4];[1;2;3]]
   let rm' = mkMatrix r
   let _ = match rm' with
     | Ok _ -> assert false
     | Error e -> assert (e = UnevenRows)

   ************

   Task 2: Implement the function `transpose` which, given

     m : a matrix

   returns the transpose of `m`.

   Example:
   let _ = match rm with
     | Ok m ->
       let tm = transpose m in
       let _ = assert (tm.num_rows = 3) in
       let _ = assert (tm.num_cols = 2) in
       let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
       ()
     | _ -> assert false

   ************

   Task 3: Implement the function `multiply` which, given

     m : a matrix
     n : a matrix

   returns the product of `m` and `n` if it is possible to multiply
   them. The error condition:

   * If the number of columns of `m` is not the same as the number of
   rows of `n`, then `multiply` should return `Error MulMismatch`

   Example:
   let a =
     { num_rows = 2 ;
       num_cols = 2 ;
       rows = [[1.;2.];[3.;4.]] ;
     }

   let b =
     { num_rows = 1 ;
       num_cols = 2 ;
       rows = [[1.; 2.]] ;
     }

   let _ = assert (multiply a a = Ok {
     num_rows = 2 ;
     num_cols = 2 ;
     rows = [[7.;10.];[15.;22.]] ;
    })

   let _ = assert (multiply a b = Error MulMismatch)

   ************

   References:
   https://en.wikipedia.org/wiki/Matrix_multiplication
   https://en.wikipedia.org/wiki/Transpose
   https://www.cs.bu.edu/fac/crovella/cs132-book/landing-page.html

*)

type error
   = UnevenRows
   | ZeroRows
   | ZeroCols
   | MulMismatch

type 'a matrix = {
  num_rows : int ;
  num_cols : int ;
  rows : ('a list) list ;
}

let mkMatrix (rs : 'a list list) : ('a matrix, error) result =
  match rs with
  | [] -> Error ZeroRows
  | h :: t ->
      let row_length = List.length h in
      if row_length = 0 then Error ZeroCols
      else
        let rec check_rows rows acc =
          match rows with
          | [] -> Ok { num_rows = List.length acc; num_cols = row_length; rows = acc }
          | h :: t ->
              if List.length h <> row_length then Error UnevenRows
              else check_rows t (acc @ [h])
        in
        check_rows t [h]
let transpose (m : 'a matrix) : 'a matrix =
  let rec help rows i =
    match rows with
    |[] ->[]
    |h :: t->(List.nth h i) :: (help t i)
  in
  let rec helper rows num_cols i = 
    if i < num_cols then help rows i::helper rows num_cols (i+1)
    else []
  in{num_rows = m.num_cols; num_cols = m.num_rows; rows = helper m.rows m.num_cols 0}
let multiply (m : float matrix) (n : float matrix) : (float matrix, error) result =
  (* if m.num_cols <> n.num_rows then Error MulMismatch
  else
    let rec dot_product row col =
      match row, col with
      | [], [] -> 0.
      | h1 :: t1, h2 :: t2 -> h1 *. h2 +. dot_product t1 t2
      | _, _ -> failwith "Mismatched lengths" 
     *)
     assert false