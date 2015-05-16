let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let ints = read_lines () in
    let answer = List.fold_left (+) 0 (List.filter (fun x -> x mod 2 <> 0) ints) in
    print_int answer; print_newline()
