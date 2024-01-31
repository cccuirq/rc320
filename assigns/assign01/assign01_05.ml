(* Block text

   Please implement the function `block_text` of type `string ->
   string` which, given

   - a string `s` consisting only of capital English characters A-Z
   - a nonnegative integer `min_width`
   - a positive integer `max_width`

   returns a string with the same characters as `s` but separated into
   lines with the following properties:

   - each line has length at most `max_width`
   - each line has the same length except possibly the last line
   - the last line has length at least `min_width`

   If the above three properties are not possible to satisfy, then
   every line except for the last line of the returned string should
   have length `max_width` (in other words, ignore the last
   condition).

   If there are multiple ways to satisfy the above three properties,
   the choice with the longest lines (besides the last line) should be
   used.

   Hint: Take a look at the following functions in the OCaml
   documentation:

   - `String.sub`
   - `String.length`

   Examples:
   let _ = assert (block_text "ABCDEFGHIJ" 0 3 = "ABC\nDEF\nGHI\nJ")
   let _ = assert (block_text "ABCDEFGHIJ" 2 3 = "AB\nCD\nEF\nGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 0 4 = "ABCD\nEFGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 3 4 = "ABCD\nEFGH\nIJ")

 *)

    let block_text (s : string) (min_width : int) (max_width : int) : string =
      let rec count s news min_width count_max =
        let fi s count_max = 
          String.sub s 0 count_max
        in
        if(String.length s <= max_width) then news ^ "\n" ^ s (*if smaller than or equal to max width means we can leave the rest and append it*)
        else if (String.length news) < 1 then count (String.sub s (count_max) (String.length s - count_max)) ((fi s count_max)) min_width count_max
          (*if the length of the new string is smaller than 1 means we can add the first part directly and recursively since this is our first part*)
        else count (String.sub s (count_max) (String.length s - count_max)) ((news ^ "\n" ^ fi s count_max)) min_width count_max
          (*other situations we need to append the next substring to our current new subtring with \n*)
      in
        let rec new_max s curr_max min_width =
          (*to find approriate max_width*)
          if curr_max < min_width then max_width (*if current max_width is even smaller than min_width, then max_width - base case*)
          else if ((String.length s) mod curr_max) = 0 then curr_max(*if the length of string s mod current max_width = 0 means we find most appropriate max_width*)
          else if ((String.length s) mod curr_max) >= min_width then curr_max
          (*correct base case according to the question*)
          else new_max s (curr_max - 1) min_width
          (*inappropriate max, try others*)
        in
        if (String.length s) < max_width then s (*base case*)
        else if (min_width = 0) then count s "" 1 max_width  (*if min_width is 0 then we can seperate our string directly*)
        else if (String.length s) mod max_width < min_width then count s "" min_width (new_max s max_width min_width)
          (*if the length of our string mod the max_width is smaller than the min width, means we need a more appropriate max_width*)
        else count s "" min_width max_width
        (*else keep the same*)
