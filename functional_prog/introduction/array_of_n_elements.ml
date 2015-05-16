let make_array n =
   Array.make n n

let () =
    let len = read_int () in
    let arr = make_array len in
    Printf.printf "%d\n" (Array.length arr)
