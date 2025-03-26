module MakeQuadraticSieve (Row : Types.RowType) = struct
  let xor a b = (a || b) && not (a && b)
  let row_xor = Row.xor
  let create = Row.create
  let set = Row.set
  let get = Row.get
  let all_zeros = Row.all_zeros
  let emptyRow = Row.empty
  let idxSet t = Row.idxSet t

  let read_file_T name = 
    let n = ref (Big_int.big_int_of_int 0) in
    let b = ref 0 in
    let tab = ref [] in
    let different_factors_list = ref [||] in
    let amount_different_factors = ref 0 in
    let rows = ref 0 in
    
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
            rows := !rows +1;
            match split with
            | l :: r :: [] -> 
                              begin
                                let big = Big_int.big_int_of_string l in
                                let num_list = 
                                  match String.split_on_char ' ' r with
                                  | [] -> []
                                  | _ :: ll -> List.map (  
                                    fun s ->
                                      let i = int_of_string s in  
                                          if (Array.mem i !different_factors_list) then i
                                          else (
                                              different_factors_list := Array.append !different_factors_list [|i|];
                                              amount_different_factors:=!amount_different_factors+1;
                                              i
                                              )
                                      ) ll
                                in
                                tab := (big, num_list) :: !tab
                              end
            | _ -> failwith "Line of the file not built correctly"
          done
        with End_of_file -> close_in channel
      with
      | Sys_error msg -> failwith ("File could not be opened: " ^ msg)
      | e -> raise e
    in
    let idxHash = Hashtbl.create 128 in
    let idx = ref 0 in
    Array.sort Int.compare !different_factors_list;
    Array.iter (fun value -> 
      Hashtbl.replace idxHash value !idx; 
      idx:=(!idx)+1) !different_factors_list;
    (!n, !b, !tab, !amount_different_factors, !different_factors_list, idxHash, !rows)
  ;;

  let new_gauss z rows cols = 
    (* swaps rows of index i and j for both z and t matrix *)
    let swap_rows z t i j =
      if i<>j then
        (
        let temp1 = z.(i) in 
        z.(i) <- z.(j);
        z.(j) <- temp1;
        let temp2 = t.(i) in 
        t.(i) <- t.(j);
        t.(j) <- temp2;
        )
    in

    (* t is the matrix identity of size rows*rows *)
    let t = 
      Array.init (rows) 
      (
        fun i -> let b = create rows in 
                set b i true;
                b
      )
    in

    (* actual pivot *)
    for j=0 to min (rows - 2) (cols - 2) do 
      (
      (* for each column *) 
      for i=j to (rows-1) do
        (* for each line *)
        (
          (*finding pivot*)
          let continue = ref true in 
          if (get z.(i) j && !continue) then begin
            (*swap columns*)
            (*i is the pivot index, j is the current column, pivot row must be swaped with j row in 'z'; 't'*)
            swap_rows z t i j;
            for id=i+1 to (rows-1) do 
              if get z.(id) j then
                (
                  row_xor z.(id) z.(j);
                  row_xor t.(id) t.(j);
                  continue:= false
                )
            done;
          end
        )
      done;
      )
    done;
    (z, t)
  ;;

  let new_get_matrice_Z_from_list_T list amount_factors diff_factor_list idxHash rows =
    let res = Array.make rows emptyRow in
    let bigRes = Array.make rows (Big_int.zero_big_int) in
    List.iteri
      (
        fun i (big, curr_facteurs_list) ->
          let vector = 
            let v = create amount_factors in
            List.iter 
              (
                fun value ->
                  let nth = Hashtbl.find idxHash value in
                  set v nth (xor (get v nth) true) 
              ) 
              curr_facteurs_list;
            v 
          in
          res.(i) <- vector;
          bigRes.(i) <- big;
      )
      list;
    (res, bigRes) (* Z matrix, their associated Big Int*)
  ;;

  let get_gcd_from_z_t z t bigNums rows n tab = 
    let gcd_answer n x y =
      let gcd a b =
        Big_int.gcd_big_int a b
      in
      gcd (Big_int.sub_big_int x y) n
    in

      let ans = ref None in
      for i=0 to (rows-1) do
        if (Option.is_none !ans) then
          if (all_zeros z.(i)) then 
            (*found an answer*)
            let all_rows_needed = idxSet t.(i) in
            (*calculating X and Y -needed for gcd- *)
            let x = ref (Big_int.unit_big_int) in 
            let y = ref (Big_int.unit_big_int) in 

            List.iter
            (
              (* For each row needed, multiply all factors of the given row into y *)
              fun idx ->
                let curr_big = bigNums.(idx) in
                x := Big_int.mult_big_int (!x) curr_big;
                (* searching for factors with curr_big as the big num *)
                let rec loop big_num l =
                  match l with 
                  | [] -> failwith "Issue in the database logic"
                  | (e, factors) :: ll ->
                    if Big_int.eq_big_int e big_num then 
                      factors
                    else
                      loop big_num ll
                in
                let factors = loop curr_big tab in 
                List.iter (fun v -> y:= Big_int.mult_int_big_int v (!y)) factors
            ) 
            all_rows_needed;
            y:= Big_int.sqrt_big_int (!y);
            
            (*Printf.printf "X=%s |Y=%s \n" (Big_int.string_of_big_int !x)(Big_int.string_of_big_int !y);*)
            
            let gcd_answer_var = (gcd_answer n !x !y) in
            (* if gcd isn't 1 or n itself then answer *)
            if (not (Big_int.eq_big_int gcd_answer_var (Big_int.unit_big_int))
                && not (Big_int.eq_big_int gcd_answer_var n)) then 
              ans:=Some gcd_answer_var
      done;
      !ans
  ;;

  let resolution_from_file_T n b tab amount_factors diff_factor_list hash number_of_rows timeShowed =
    match timeShowed with 
    | None -> 
      let pre_time = Unix.gettimeofday () in
      let (matriceZ, bigNumbers) = new_get_matrice_Z_from_list_T tab amount_factors diff_factor_list hash number_of_rows in
      let (z,t) = new_gauss matriceZ number_of_rows amount_factors in
      let gcd = get_gcd_from_z_t z t bigNumbers number_of_rows n tab in
      (
      match (gcd) with
      | None -> failwith "No solution found, shouldn't happen (ending resolution)"
      | Some gcd ->
          Printf.printf "%s = %s * %s\n%!" (Big_int.string_of_big_int n) (Big_int.string_of_big_int (Big_int.div_big_int n gcd)) (Big_int.string_of_big_int gcd);
      let post_time = Unix.gettimeofday () in 
      Printf.printf "Total time : %f\n%!" (post_time-.pre_time);
      )


    | Some _ ->
      Printf.printf ("\nStep 1..\n%!");
      let pre_time = Unix.gettimeofday () in
      let beginning = pre_time in
      let (matriceZ, bigNumbers) = new_get_matrice_Z_from_list_T tab amount_factors diff_factor_list hash number_of_rows in
      let post_time = Unix.gettimeofday () in 
      Printf.printf "Running time : %f\n%!" (post_time-.pre_time);

      Printf.printf ("\nStep 2..\n%!");
      let pre_time = Unix.gettimeofday () in
      let (z,t) = new_gauss matriceZ number_of_rows amount_factors in
      let post_time = Unix.gettimeofday () in 
      Printf.printf "Running time : %f\n%!" (post_time-.pre_time);

      Printf.printf ("\nStep 3..\n%!");
      let pre_time = Unix.gettimeofday () in
      let gcd = get_gcd_from_z_t z t bigNumbers number_of_rows n tab in 
      (* using GCD to find an answer *)
      let post_time = Unix.gettimeofday () in 
      Printf.printf "Running time : %f\n%!" (post_time-.pre_time);

      let post_time = Unix.gettimeofday () in 
      Printf.printf "\nTotal time : %f\n\n%!" (post_time-.beginning);
      
      match (gcd) with
      | None -> failwith "No solution found, shouldn't happen (ending resolution)"
      | Some gcd ->
          Printf.printf "%s = %s * %s\n%!" (Big_int.string_of_big_int n) (Big_int.string_of_big_int (Big_int.div_big_int n gcd)) (Big_int.string_of_big_int gcd);
  ;;

  let full_resolution file_name =
    let (n, b, tab, amount_factors, diff_factor_list, hash, rows) = read_file_T ("src/test/"^(file_name)^".txt") in
    resolution_from_file_T n b tab amount_factors diff_factor_list hash rows None

end