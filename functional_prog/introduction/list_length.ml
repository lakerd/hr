let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let size lst =
    let rec s lst acc =
        match lst with
    [] -> acc
    | x::xs -> s xs (1+acc)
    in s lst 0

let () =
    let ints = read_lines () in
    let s = size ints in
    print_int s; print_newline ()
