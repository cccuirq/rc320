let f x = x + 2
let y = "two"

(*using utop in terminal to open a file by #*)

let parity_str x =
  if x < 0 then "negative" else if x > 0 then "positive" else "zero"

let _ = assert(parity_str 2 = "positive")
let _ = assert(parity_str (-2) = "positive")

let squred x1 y1 x2 y2 =
    let x_diff = x1 - x2 in
    let y_diff = y1 - y2 in
    x_diff * x_diff - y_diff - y_diff