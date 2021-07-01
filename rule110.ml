let rec get_last_element my_list = 
    match my_list with
    | a :: [] -> a
    | a :: tl -> get_last_element tl
    | _ -> -1



let cell_merger a b c =
    match (a, b, c) with
    |(1 , 1 , 1) -> 0
    |(1 , 1 , 0) -> 1
    |(1 , 0 , 1) -> 1
    |(1 , 0 , 0) -> 0
    |(0 , 1 , 1) -> 1
    |(0 , 1 , 0) -> 1
    |(0 , 0 , 1) -> 1
    |(0 , 0 , 0) -> 0
    | _ -> -1



let print_state state = 
    List.iter (fun a -> if a = 0 then  Printf.printf " " else Printf.printf "█" ) state


let calculate_state old_state last_cell = 

    (*Add Exceptions*)
    let (first, rest) = 
        match old_state with
        | a :: b :: tl ->  (cell_merger last_cell a b, a:: b :: tl)
        | _ -> raise (Failure "Invalid state size")

    in

    (*Returns the New State but reversed*)
    let rec calculate_cell old_state_temp new_state_temp =
        match old_state_temp with 
        | a::b::[] ->
                let new_last_cell = cell_merger a b (List.hd old_state) in
                (new_last_cell :: new_state_temp)
        | a :: b :: c :: tl -> calculate_cell (b::c::tl) ((cell_merger a b c) :: new_state_temp)
        | _-> []
    in calculate_cell rest (first :: [])
    


let rec loop current_state previous_last_cell loops_left =
    if loops_left = 0 then
        ()
    else
        let new_state_reversed = calculate_state current_state previous_last_cell in
        let new_last_cell = List.hd new_state_reversed in
        let new_state = List.rev new_state_reversed in
        print_state new_state;
        print_newline ();
        loop new_state new_last_cell (loops_left - 1)

let string_to_list str =
    let temp_array = Array.make (String.length str) 0 in
    String.iteri (fun i c -> Array.set temp_array i (
        if c = '0' then 0 else 1
    ) ) str;
    Array.to_list temp_array


let () = 

    let times_to_loop = int_of_string Sys.argv.(1) in
    let first_state = string_to_list Sys.argv.(2) in
    let first_last_cell = get_last_element first_state in
    print_state first_state;
    print_newline ();
    loop first_state first_last_cell times_to_loop
    

