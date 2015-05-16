let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec rev arr acc =
    match arr with
    | [] -> acc
    | x::xs -> rev xs (x::acc)

let () =
    let arr = read_lines () in
    let r = rev arr [] in
    List.iter (fun i -> print_int i; print_newline()) r
