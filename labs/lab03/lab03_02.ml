(* String Triangle

   Implement the function `string_tri` of type `string -> string`
   which given

   s : string conisting only of capital english letters [A-Z]

   returns the string with the same characters but organized as a
   right triangle.  If the last line as too few letters, then pad it with '*'.

   Examples:
   let _ = assert (string_tri "ABCDE" = "A\nBC\nDE*")
   let _ = assert (string_tri "ABCDEFGH" = "A\nBC\nDEF\nGH**")
   let _ = assert (string_tri "AAA" = "A\nAA")

*)

let string_tri s =
   let rec help s n = 
      let len = String.length s in
      if n <= len then s ^ String.init (n - len) (fun _ -> '*')
      else (String.sub s 0 n) ^ "\n" ^ help (String.sub s n len) (n+1)
      in 
   help s 1
