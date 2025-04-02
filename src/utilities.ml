module MakeQuadraticSieve (Row : Types.RowType) = struct
  open Row
  let xor_int a b = (a || b) && not (a && b)

  (* 
  This function reads a file similar to T_ files - see test folder -, and returns every usefull informations from it

  Parameters : 
    - File name (it must be similar to how T_ files are built)
    - Optimization option (True for optimized version, False for old version)
  Return value :
    - (N, b, tab, amount_different_factors, idxHash, rows) :
          - N is the number we are working on
          - b is used in order to create all the equations (not used in this program)
          - tab is a list of all pairs of (BigNumber * (list of factors)) - based on each line of the file -
          - amount_different_factors is the number of columns - which is the amount of unique factors in it -
          - idxHash is a Hash table associating every column index to its corresponding factor
          - rows is the amount of rows in it
  *)
  let read_file_T name is_opti = 
    let n = ref (Big_int.big_int_of_int 0) in
    let b = ref 0 in
    let tab = ref [] in
    let different_factors_list = ref [||] in
    let amount_different_factors = ref 0 in
    let rows = ref 0 in
    let index_count = Hashtbl.create 128 in
    
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
        (*Rest of the file*)
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

                                          if is_opti then
                                            (
                                              ignore (match (Hashtbl.find_opt index_count i) with 
                                              | None -> Hashtbl.replace index_count i 1
                                              | Some acc -> Hashtbl.replace index_count i (acc+1))
                                            );

                                          if (Array.mem i !different_factors_list) then i
                                          else 
                                            (
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
      | Sys_error msg -> failwith ("File could not be opened: "^msg)
      | e -> raise e
    in

    (* is_opti option : Check if they are useless rows with only 1 column and removes them (as well as the correspondings columns) *)
    if is_opti then 
    (    
      Hashtbl.iter
        (
          fun solo_val v -> if v = 1 then 
            (
              (* Remove rows from tab *)
              tab := List.filter (fun (big, num_list) -> not (List.mem solo_val num_list)) !tab;
              if (0<(!rows)-(List.length !tab)) then
                (
                  rows:=!rows-1;
                  amount_different_factors:=!amount_different_factors-1;
                  (* Remove the factor from different_factors_list *)
                  different_factors_list:= Array.of_list (List.filter (fun x -> x != solo_val) (Array.to_list !different_factors_list));
                )
            )
        ) 
        index_count;
    );

    let idxHash = Hashtbl.create 128 in
    let idx = ref 0 in
    Array.sort Int.compare !different_factors_list;
    Array.iter (fun value -> 
      Hashtbl.replace idxHash value !idx; 
      idx:=(!idx)+1) 
      !different_factors_list;
    (!n, !b, !tab, !amount_different_factors, idxHash, !rows)
  ;;

  (* 
  This function does the Gauss algorithm on arrays of t (type of a row, which represents a bit vector)

  Parameters : 
    - z (The base Matrix we're doing the algorithm on)
    - rows (number of rows)
    - cols (number of cols)
  Return value :
    - (Z, T) :
          - Z is the final state of the base Matrix z
          - T is the final state of the matrix identity used for the algorithm
  *)
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
                  xor z.(id) z.(j);
                  xor t.(id) t.(j);
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


  (* 
  This function create the base matrix needed for the gauss algorithm (called Z Matrix)

  Parameters : 
    - tab (corresponds to the tab from the return value of the read_file_T function)
    - number_of_cols
    - idxHash (corresponds to the idxHash from the return value of the read_file_T function)
    - number_of_rows
  Return value :
    - Z (the base matrix needed for the gauss algorithm)
    - bigNumbers (big numbers associated with every row of the matrix)
  *)
  let new_get_matrice_Z_from_list_T tab number_of_cols number_of_rows idxHash =
    let res = Array.make number_of_rows empty in
    let bigRes = Array.make number_of_rows (Big_int.zero_big_int) in
    List.iteri
      (
        fun i (big, curr_facteurs_list) ->
          let vector = 
            let v = create number_of_cols in
            List.iter 
              (
                fun value ->
                  let nth = Hashtbl.find idxHash value in
                  set v nth (xor_int (get v nth) true) 
              ) 
              curr_facteurs_list;
            v
          in
          res.(i) <- vector;
          bigRes.(i) <- big;
      )
      tab;
    (res, bigRes) (* Z matrix, their associated Big Int*)
  ;;

  
  (* 
  This function calculate a relevant divider of the number N, given the informations from other functions

  Parameters : 
    - Z (final Z Matrix from the gauss algorithm)
    - T (final T Matrix from the gauss algorithm)
    - bigNums (corresponds to the bigNumbers from the return value of the new_get_matrice_z_from_list_T function)
    - rows (amount of rows)
    - n (the N number we're working on)
    - tab (corresponds to the tab from the return value of the read_file_T function)
  Return value :
    - Some answer (one of the two divider from an answer), or None if no answer was found (shouldn't happen)
  *)
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

  
  (* 
  This function handles all the process that leads to the final dividers of N

  Parameters : 
    - n (the N number we're working on)
    - tab (corresponds to the tab from the return value of the read_file_T function)
    - number_of_cols
    - number_of_rows
    - hash (corresponds to the hashIdx from the return value of the read_file_T function)
    - timeShowed (option to display to time needed for each function)
  *)
  let resolution_from_file_T n tab number_of_cols number_of_rows hash timeShowed =
    match timeShowed with 
    | None -> 
      let pre_time = Unix.gettimeofday () in
      let (matriceZ, bigNumbers) = new_get_matrice_Z_from_list_T tab number_of_cols number_of_rows hash in
      let (z,t) = new_gauss matriceZ number_of_rows number_of_cols in
      let gcd = get_gcd_from_z_t z t bigNumbers number_of_rows n tab in
      let post_time = Unix.gettimeofday () in
      (
      match (gcd) with
      | None -> failwith "No solution found, shouldn't happen (ending resolution)"
      | Some gcd ->
          Printf.printf "%s = %s * %s\n%!" (Big_int.string_of_big_int n) (Big_int.string_of_big_int (Big_int.div_big_int n gcd)) (Big_int.string_of_big_int gcd);
          Printf.printf "Total time : %f\n%!" (post_time-.pre_time);
      )


    | Some _ ->
      Printf.printf ("\nnew_get_matrice_Z_from_list_T..\n%!");
      let pre_time = Unix.gettimeofday () in
      let beginning = pre_time in
      let (matriceZ, bigNumbers) = new_get_matrice_Z_from_list_T tab number_of_cols number_of_rows hash in
      let post_time = Unix.gettimeofday () in 
      Printf.printf "Running time : %f\n%!" (post_time-.pre_time);

      Printf.printf ("\nnew_gauss..\n%!");
      let pre_time = Unix.gettimeofday () in
      let (z,t) = new_gauss matriceZ number_of_rows number_of_cols in
      let post_time = Unix.gettimeofday () in 
      Printf.printf "Running time : %f\n%!" (post_time-.pre_time);

      Printf.printf ("\nget_gcd_from_z_t..\n%!");
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

  let full_resolution file_name is_opti timeShowed =
    let (n, _, tab, cols, hash, rows) = read_file_T ("src/test/"^(file_name)^".txt") is_opti in
    resolution_from_file_T n tab cols rows hash timeShowed

end