let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec filter n lst =
    match lst with
        | [] -> []
        | x :: xs -> if x < n then x :: (filter n xs) else filter n xs

let () =
    let n::arr = read_lines () in
    let x = filter n arr in
    List.iter (fun i -> print_int i; print_newline()) x
