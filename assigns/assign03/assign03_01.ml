(* Concatenation Lists

   A `concatlist` is a list whose constructors are based on
   concatenation instead of cons.  It has a constructor for the empty
   list (Nil), a single element list (Single) and the concatentation
   of two lists (Concat).

   Implement a function `sort` which given

     l : a `concatlist`

   returns a regular list with the same element as `l` but in sorted
   order.  You should do this WITHOUT trying to first converting `l`
   into a regular list.  In particular, you CANNOT use the function
   `List.sort`.

   Example:
   let l = Concat (Concat (Single 3, Single 2), Concat (Single 1, Single 10))
   let _ = assert (sort l = [1;2;3;10])

*)

type 'a concatlist
  = Nil
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist

let sort (l : 'a concatlist) : 'a list =
  let rec sort_addend x curr = 
    match curr with
    | [] -> [x] 
    | h :: t ->
        if x <= h then
          x :: curr 
        else
          h :: (sort_addend x t)  
 in
  let rec helper l my = 
    match l with
      | Nil -> my
      | Single x -> helper Nil (sort_addend x my)
      | Concat (left, right) -> 
        let le = helper left my in 
        helper right le
  in 
  helper l []