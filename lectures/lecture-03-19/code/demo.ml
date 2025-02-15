
let cons x xs = x :: xs
let explode (s : string) : char list = String.fold_right cons s []
let implode (cs : char list) : string = String.init (List.length cs) (List.nth cs)
let is_digit (c : char) : bool = List.mem c (explode "0123456789")
let is_whitespace (c : char) : bool = List.mem c (explode " \r\n\t")

let rec take_while (p : 'a -> bool) (l : 'a list) =
  match l with
  | [] -> []
  | (x :: xs) -> if p x then x :: take_while p xs else []

let rec drop_while (p : 'a -> bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | (x :: xs) -> if p x then drop_while p xs else x :: xs

(* Simple Grammar

   <expr>  ::= <expr2> {( + | - ) <expr2>}
   <expr2> ::= <expr3> {( * | / ) <expr3>}
   <expr3> ::= <int> | ( <expr> )
   <int>   ::= <digit> {<digit>}
   <digit> ::= 0|1|2|3|4|5|6|7|8|9

*)

type token
  = LParT
  | RParT
  | MulT
  | DivT
  | AddT
  | SubT
  | IntT of int
  | EOFT

let rec next_token (cs : char list) : (token * char list) option =
  match cs with
  | '(' :: rest -> Some (LParT, rest)
  | ')' :: rest -> Some (RParT, rest)
  | '*' :: rest -> Some (MulT, rest)
  | '/' :: rest -> Some (DivT, rest)
  | '+' :: rest -> Some (AddT, rest)
  | '-' :: rest -> Some (SubT, rest)
  | c :: rest when is_whitespace c -> next_token rest
  | c :: rest when is_digit c ->
    let digits_str = implode (take_while is_digit cs) in
    Some (IntT (int_of_string digits_str), drop_while is_digit cs)
  | [] -> Some (EOFT, [])
  | _ -> None

let tokenize
    (next : char list -> ('a * char list) option)
    (s : string) : token list option =
  let rec go cs =
    if List.is_empty cs
    then Some []
    else
      match next cs with
      | None -> None
      | Some (t , rest) ->
        match go rest with
        | None -> None
        | Some ts -> Some (t :: ts)
  in go (explode s)

(* A Simpler Grammar (right-associative addition with parentheses)

   <expr> = <expr2> | <expr2> + <expr2>
   <expr> = <int> | ( <expr> )

*)
(* type token1 
= LParT
|RParT
|AddT
|Num of int

let rec next_token1 (cs: char list) : (token1*char list) option =
  match cs with
  | ' ' :: rest -> next_token1 rest 
  |'+' :: rest -> Some(AddT,rest)
  | '(' :: rest -> Some (LParT, rest)
  | ')' :: rest -> Some (RParT, rest)
  | c :: rest when is_digit c ->
    let num_str = take_while is_digit cs in
    let rest = drop_while is_digit cs in
    let num = int_of_string (implode num_str) in 
    Some (Num num, rest)
  | _ -> None *)


type expr
  = Num of int
  | Add of expr * expr

let rec parseExpr (ts : token list) : (expr * token list) option =
  match parseExpr2 ts with
  | None -> None
  | Some (larg, rest) ->
    match rest with
    | AddT :: rest ->
      (match parseExpr rest with
       | None -> None
       | Some (rarg, rest) -> Some (Add (larg, rarg), rest))
    | _ -> Some (larg, rest)
and parseExpr2 ts =
  match ts with
  | IntT x :: rest -> Some (Num x, rest)
  | LParT :: rest ->
    (match parseExpr rest with
     | None -> None
     | Some (e, rest) ->
       match rest with
       | RParT :: rest -> Some (e, rest)
       | _ -> None)
  | _ -> None

let parse s =
  match tokenize next_token s with
  | None -> None
  | Some ts ->
    match parseExpr ts with
    | Some (t, []) -> Some t
    | Some (t, [EOFT]) -> Some t
    | _ -> None
