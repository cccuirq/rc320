(* Variant for arithemtic expressions *)

type expr
  = Val of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr

let x = Add (Val 3, Sub (Mul (Val 2, Val 4), Val 14))

let rec eval (e : expr) : int = 
  match e with
  |Val n -> n
  |Add (a, b) -> eval a + eval b
  |Sub (a, b) -> eval a - eval b
  |Mul (a, b) -> eval a * eval b


let _ = assert (eval x = -3)

