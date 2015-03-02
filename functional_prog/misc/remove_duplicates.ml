let rec find_size str str_size table n acc newsize =
    if n = str_size then
        newsize, (List.rev acc)
    else
        let m = str.[n] in
        if Hashtbl.mem table m then
            find_size str str_size table (n+1) acc newsize
         else
            begin 
                Hashtbl.add table m 0;
                find_size str str_size table (n+1) (m::acc) (newsize+1)
            end
            
let get_str newsize chars =
    let rec chars_to_str chars s n =
        match chars with
        | [] -> s
        | x::xs -> s.[n] <- x; chars_to_str xs s (n+1)
        in 
    chars_to_str chars (String.make newsize 'a') 0

let () = 
    let s = read_line () in
    let m = Hashtbl.create 32 in
    let newsize, chars = find_size s (String.length s) m 0 [] 0 in
    Printf.printf "%s\n" (get_str newsize chars) 
