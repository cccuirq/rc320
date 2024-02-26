let rec foo l =
    match l with
    | [] -> []
    | false :: bs ->
    List.map (fun x -> x - 1) (0 :: foo bs)
    | true :: bs -> bar l
    and bar l =
    match l with
    | [] -> []
    | false :: bs -> foo l
    | true :: bs -> List.map ((+) 1) (0 :: bar bs)

let rev ls =
    let rec helper lss acca = 
        match lss with
        | [] -> acca
        | x :: xs -> helper xs (x :: acca)
    in
    let rec help ls acc =
        match ls with
        | [] -> acc
        | xs :: ys -> help ys (helper xs acc)
    in help ls []