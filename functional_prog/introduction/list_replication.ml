let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []


let rec dup n x acc =
   if n = 0 then
        acc
   else
        dup (n-1) x (x::acc)

let rec f n arr acc =
    match arr with
      | [] -> List.rev acc
      | x :: xs ->
            f n xs (dup n x [] @ acc)

let () =
    let n::arr = read_lines() in
    let ans = f n arr [] in
    List.iter (fun x -> print_int x; print_newline ()) ans;;
