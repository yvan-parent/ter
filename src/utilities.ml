let read_file_T name = 
  let n = ref (Big_int.big_int_of_int 0) in
  let b = ref 0 in
  let tab = ref [] in
  let () =
    try
      let channel = open_in name in
      (*First line*) 
      let line1 = input_line channel in
      let split = String.split_on_char ' ' line1 in 
      match split with 
      | l :: r :: [] -> n:= Big_int.big_int_of_string l; b:= int_of_string r
      | _ -> failwith "First line of the file not built correctly";
      ;
      (*Rest of file*)
      try 
        while (true) do 
          let line = input_line channel in 
          let split = String.split_on_char ':' line in 
          match split with
          | l :: r :: [] -> 
                            begin
                              let big = Big_int.big_int_of_string l in 
                              let rec aux liste =
                                match liste with
                                | [] -> []
                                | e :: ll -> [int_of_string e] :: (aux ll)
                              in
                              let liste = match (String.split_on_char ' ' r) with | [] -> [] | _ :: ll -> ll in
                              tab:= (big, (aux liste)) :: !tab 
                            end
          | _ -> failwith "Line of the file not built correctly"
        done
      with End_of_file -> close_in channel
    with
    | Sys_error msg -> failwith ("File could not be opened: " ^ msg)
    | e -> raise e
  in
  (!n, !b, !tab)
;;