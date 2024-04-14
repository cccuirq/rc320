(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'

type 'a parser = char list -> ('a * char list) option

let satisfy f = function
  | c :: cs when f c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str s cs =
  let rec go = function
    | [], ds -> Some (s, ds)
    | c :: cs, d :: ds when c = d -> go (cs, ds)
    | _ -> None
  in go (explode s, cs)

let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None

let (>|=) p f = map f p
let (>|) p x = map (fun _ -> x) p

let seq p1 p2 cs =
  match p1 cs with
  | Some (x, cs) -> (
      match p2 cs with
      | Some (y, cs) -> Some ((x, y), cs)
      | None -> None
    )
  | None -> None

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

let map2 f p1 p2 =
  seq p1 p2 >|= fun (x, y) -> f x y

let optional p cs =
  match p cs with
  | Some (x, cs) -> Some (Some x, cs)
  | None -> Some (None, cs)

let rec many p cs =
  match p cs with
  | Some (x, cs) -> (
      match (many p cs) with
      | Some (xs, cs) -> Some (x :: xs, cs)
      | None -> Some ([x], cs)
    )
  | None -> Some ([], cs)

let many1 p = map2 cons p (many p)

let alt p1 p2 cs =
  match p1 cs with
  | Some x -> Some x
  | None ->
    match p2 cs with
    | Some x -> Some x
    | None -> None

let (<|>) = alt

let pure x cs = Some (x, cs)
let fail _ = None

let bind p f cs =
  match p cs with
  | Some (x, cs) -> f x cs
  | None -> None

let (>>=) = bind

let choice ps =
  List.fold_left (<|>) fail ps

let ws = many (satisfy is_blank)
let keyword w = str w << ws

let rec_parser p =
  pure () >>= p

let parse p s =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* END OF UTILITIES *)

(* ============================================================ *)

(* BEGINNING OF PROJECT CODE *)

type ident = string
type command
  = Drop                   (* drop *)
  | Swap                   (* swap *)
  | Dup                    (* dup *)
  | Trace                  (* . *)
  | Add                    (* + *)
  | Sub                    (* - *)
  | Mul                    (* * *)
  | Div                    (* / *)
  | Lt                     (* < *)
  | Eq                     (* = *)
  | Bind of ident          (* |> ID *)
  | Call of ident          (* # ID *)
  | If of program          (* ? prog ; *)
  | Def of ident * program (* def prog ; *)
  | Ident of ident         (* ID *)
  | Num of int             (* num *)
and program = command list

let parse_ident = 
  ws >> many1 (satisfy is_upper_case) >>= fun id_chars ->
  ws >> pure (implode id_chars)

let parse_drop = ws >> (keyword "drop" << ws) >> pure Drop
let parse_swap = ws >> (keyword "swap" << ws) >> pure Swap
let parse_dup = ws >> (keyword "dup" << ws) >> pure Dup
let parse_trace = ws >> (keyword "." << ws) >> pure Trace
let parse_add = ws >> (keyword "+" << ws) >> pure Add
let parse_sub = ws >> (keyword "-" << ws) >> pure Sub
let parse_mul = ws >> (keyword "*" << ws) >> pure Mul
let parse_div = ws >> (keyword "/" << ws) >> pure Div
let parse_lt = ws >> (keyword "<" << ws) >> pure Lt
let parse_eq = ws >> (keyword "=" << ws) >> pure Eq
let parse_num =
  ws >> many1 (satisfy is_digit) >>= fun digits ->
  ws >> pure (Num (int_of_string (implode digits)))

(* You are not required to used this but it may be useful in
   understanding how to use `rec_parser` *)
let rec parse_com () =
  let parse_def =
    map2
      (fun id p -> Def (id, p))
      (ws >> (keyword "def" >> parse_ident) << ws)
      (parse_prog_rec() << char ';')
  in
  let parse_bind = 
    ws >> keyword "|>" >> (parse_ident << ws) >>= fun id -> pure (Bind id)
  in
  let parse_call = 
    ws >> keyword "#" >> (parse_ident << ws) >>= fun id -> pure (Call id)
  in
  let parse_if = 
    ws >> keyword "?" >> parse_prog_rec() << char ';' >>= fun id -> pure (If id)
  in
  let parse_ident_command =
    parse_ident >>= fun id -> pure (Ident id)
  in
  parse_def <|> parse_drop <|> parse_swap <|> parse_dup <|> parse_trace <|> parse_add
  <|> parse_sub <|> parse_mul <|> parse_div <|> parse_lt <|> parse_eq
  <|> parse_bind <|> parse_call <|> parse_if <|> parse_ident_command <|> parse_num
and parse_prog_rec () =
  many ((rec_parser parse_com) << ws)

let parse_prog = 
parse (many (parse_com ()) >>= fun commands ->
pure commands)

(* A VERY SMALL TEST SET *)
(*
let test = parse_prog "drop"
let out = Some [Drop]
let _ = assert (test = out)

let test = parse_prog "     .       "
let out = Some [Trace]
let _ = assert (test = out)

let test = parse_prog "  |> TEST   "
let out = Some [Bind "TEST"]
let _ = assert (test = out)

let test = parse_prog "  23 00345 + |> OK "
let out = Some [Num 23; Num 345; Add; Bind "OK"]
let _ = assert (test = out)

let test = parse_prog "  def NEG 0 - ; 2 #NEG 2 =    \n\n   "
let out = Some [Def ("NEG", [Num 0; Sub]); Num 2; Call "NEG"; Num 2; Eq]
let _ = assert (test = out)

let test = parse_prog "
  def ABS
    dup 0 swap < ?
      0 -
    ;
  ;

  30 0 -
  #ABS
  |> X
"
let out = Some
    [ Def ("ABS", [Dup; Num 0; Swap; Lt; If [Num 0; Sub]])
    ; Num 30; Num 0; Sub
    ;  Call "ABS"
    ;  Bind "X"
    ]
let _ = assert (test = out)
*)

(* EVALUATION *)

type stack = int list
type value
  = Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list

let update_env e x v = 
  let rec update acc = function
    | [] -> List.rev ((x, v) :: acc)  
    | (y, value) :: ys when y = x -> update acc ys  
    | binding :: ys -> update (binding :: acc) ys 
  in
  update [] e 
let rec fetch_env e x = 
  match e with
  | [] -> None
  | (id, value) :: rest ->
    if id = x then Some value
    else fetch_env rest x 
  
let drop s e t p =
  match s with
  | _ :: s_tail -> (s_tail, e, t, p)
  |[] -> ([], e, "panic: nothing to drop" :: t, [])
  
let swap s e t p =
  match s with
  | x :: y :: s_tail -> (y :: x :: s_tail, e, t, p)
  | x :: [] -> ([x] , e, "panic: only one element in stack, can't swap" :: t, [])
  | [] -> ([], e, "panic: nothing to swap" :: t, [])

let duplicate s e t p =
  match s with
  | n :: s_tail -> (n :: n :: s_tail, e, t ,p)
  | [] -> ([], e, "panic: nothing to duplicate"::t,[])

let trace_cmd n s e t p =
  (s,e, (string_of_int n) :: t,p)

let push n s e t p = (n::s,e, t,p)

let add s e t p =
  match s with
  |x::y::s_tail -> ((x+y)::s_tail,e, t, p)
  |x :: [] -> ([x], e,"panic: only one can't add" :: t,[])
  |[] -> ([],e, "panic: nothing to add" :: t, [])

let subtract s e t p =
  match s with
  |x::y::s_tail -> ((x-y)::s_tail,e, t,p)
  |x :: [] -> ([x],e, "panic: only one can't subtract" :: t, [])
  |[] -> ([],e, "panic: nothing to subtract" :: t,[])

let multiply s e t p =
  match s with
  |x::y::s_tail -> ((x*y)::s_tail,e, t,p)
  |x :: [] -> ([x],e, "panic: only one can't multiply" :: t,[])
  |[] -> ([], e,"panic: nothing to multiply" :: t,[])

let divde s e t p =
  match s with
  |x :: y :: s_tail -> 
    (match y with
    |0 -> (x::0::s_tail,e, "panic: denominator 0" :: t,[])
    | _ -> ((x/y) :: s_tail,e, t,p))
  | x :: [] -> ([x], e,"panic: only one can't divide" :: t,[])
  | [] -> ([], e,"panic: nothing to divide" :: t,[])

let lessthan s e t p =
  match s with
  | x :: y :: s_tail -> 
    (match (x < y) with
    |true -> (1 :: s_tail, e,t,p)
    |false -> (0 :: s_tail,e,t,p))
  |x :: [] -> ([x], e,"panic: only one can't compare" :: t,[])
  |[] -> ([], e,"panic: nothing to compare" :: t,[])

let equals s e t p =
  match s with
  | x :: y :: s_tail -> 
    (match (x = y) with
    |true -> (1 :: s_tail,e, t,p)
    |false -> (0 :: s_tail,e,t,p))
  |x :: [] -> ([x], e,"panic: only one can't equal" :: t,[])
  |[] -> ([], e,"panic: nothing to equal" :: t,[])
  
let varass id s e t p = 
  match s with
  | n :: s_tail -> 
    let newenv = update_env e id (Num n) in
    (s_tail, newenv, t, p)
  | [] -> (s, e, "panic: empty stack for var" :: t, [])

let varfetch id s e t p =
  match fetch_env e id with
  | Some (Num n) -> (n :: s, e, t, p)
  | Some (Prog _) | None -> (s, e, "panic: fetch error" :: t, [])

let def id prog s e t p =
  (s, update_env e id (Prog prog), t, p)

let call id s e t p =
  match fetch_env e id with
  | Some (Prog pr) -> (s, e,t, pr @ p)
  | Some (Num _) | None -> (s, e, "panic: not program"::t, [])

let ifs pr s e t p =
  match s with
  |0 :: s_tail -> (s_tail, e, t, p)
  |n :: s_tail when n <> 0 -> (s_tail, e, t, pr @ p)
  |[] -> ([], e, "panic: nothing to if"::t, [])
  |_ -> failwith "should never happen"

let eval_single_command cmd (stack, env, trace, prog) =
  match cmd, stack with
  | Drop, _ -> drop stack env trace prog
  | Swap, _ -> swap stack env trace prog
  | Dup, _ -> duplicate stack env trace prog
  | Num n, _ -> push n stack env trace prog
  | Trace, n :: s_tail -> trace_cmd n stack env trace prog
  | Trace, [] -> (stack, env, "panic: empty stack on trace" :: trace,[])
  | Add , _ -> add stack env trace prog
  | Sub , _ -> subtract stack env trace prog
  | Mul, _ -> multiply stack env trace prog
  | Div, _ -> divde stack env trace prog
  | Lt, _ -> lessthan stack env trace prog
  | Eq, _ -> equals stack env trace prog
  | Bind id, _ -> varass id stack env trace prog
  | Ident id, _-> varfetch id stack env trace prog
  | Def (id, subprog), _ -> def id subprog stack env trace prog
  | Call id, _  -> call id stack env trace prog
  | If pr, _ -> ifs pr stack env trace prog
  (* | _ -> (stack, env, "panic: unimplemented command" :: trace, []) *)

let rec eval_prog s e t p =
  match p with
  | [] -> t
  | cmd :: cmds -> 
    (let (new_stack, new_env, new_trace, new_prog) = 
    eval_single_command cmd (s, e, t, cmds) in
    eval_prog new_stack new_env new_trace new_prog)


let interp input = 
  match parse_prog input with
  |Some prog -> 
    (let is = [] in
     let ie = [] in
     let it = [] in
     Some (eval_prog is ie it prog))
  |None -> None

(* END OF PROJECT CODE *)

(* ============================================================ *)

(* UNCOMMENT TO RUN INTERPRETER *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)


(* let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match interp input with
  | None -> print_endline "Parse Error"
  | Some t -> print_trace t

let _ = main () *)

let p = "
def DECR
  1 swap -
;

def HELP
dup 1 < ?
    NEW |> PRE
    OLD NEW + |> NEW
    PRE |> OLD
    #DECR
    #HELP ;
;

def FIB
  dup 0 = ? drop 0 ;
  dup 1 = ? drop 1 ;
  1 |> NEW
  0 |> OLD
  0 |> PRE
  #HELP
  drop NEW
  ;


6 #FIB ."

(* let test = interp p
let out = Some ["6"]
let _ = assert(test = out) *)