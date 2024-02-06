(* Grouping Integers and Strings

   Implement a function `convert` which given

     l : a list of `int_or_string`s

   returns a list of `int_list_or_string_list`s such that adjacent
   `int`s and `string`s are grouped together.

   Example:
   let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
   let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]
   let _ = assert (convert test_in = test_out)

*)

type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

  let convert (l : int_or_string list) : int_list_or_string_list list =
    let rec add_to_end lst x = 
      match lst with
      | [] -> [x]
      | h::t -> h :: add_to_end t x
    in
    let rec process acc current_ints current_strings left =
      match left with
      | [] -> 
          let acc = if current_ints <> [] then add_to_end acc (IntList current_ints) else acc in
          if current_strings <> [] then add_to_end acc (StringList current_strings) else acc
      | Int n :: xs ->
          if current_strings = [] then
            process acc (add_to_end current_ints n) [] xs
          else
            let acc = add_to_end acc (StringList current_strings) in
            process acc [n] [] xs
      | String s :: xs ->
          if current_ints = [] then
            process acc [] (add_to_end current_strings s) xs
          else
            let acc = add_to_end acc (IntList current_ints) in
            process acc [] [s] xs
    in
    process [] [] [] l
  
    (* let convert (l : int_or_string list) : int_list_or_string_list list =
      let rec rev avv le = 
        let rec help acc current left = 
          match left with
          |[] -> (current :: acc)
          |(Int h) :: d -> (match current with 
                            | IntList list -> help acc (IntList(h ::list)) d 
                            | _ -> help (current :: acc) (IntList [h]) d) 
          |(String h) :: d -> (match current with 
                              | StringList list -> help acc (StringList(h ::list)) d 
                              | _ -> help (current :: acc) (StringList [h]) d) 
        in match le with 
        |[] -> avv
        |h :: d -> help [] (match h with Int n -> IntList [n] | String s -> StringList [s]) d
      in rev [] l *)