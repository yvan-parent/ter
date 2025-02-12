let xor a b =
  (a || b) && not (a && b)
;;

let print_matrix m = 
  Array.iter (fun vect -> Bitv.iter (fun b -> let s = if b then "1 " else "0 " in Printf.printf "%s" s) vect; Printf.printf "\n") m
;;

let read_file_T name = 
  let n = ref (Big_int.big_int_of_int 0) in
  let b = ref 0 in
  let tab = ref [] in
  let different_factors_list = ref [||] in
  let amount_different_factors = ref 0 in
  
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
  Array.iter (fun value -> Hashtbl.replace idxHash value !idx; idx:=(!idx)+1) !different_factors_list;
  (!n, !b, !tab, !amount_different_factors, Array.sort Int.compare !different_factors_list, idxHash)
;;

let create_Z_idx_hashtbl_from_tab_T tab =
  let res = Hashtbl.create 128 in
  let inter = ref [] in 
  List.iter 
  (
    fun (big , facteurs_list) ->
      begin
        List.iter 
        (
          fun value -> 
            if (List.mem value !inter) then 
              () 
            else
              inter := value :: !inter
        )
        facteurs_list
      end
  )
  tab;
  inter := List.sort Int.compare !inter;
  let idx = ref 0 in 
  List.iter (fun value -> Hashtbl.replace res value !idx; idx:=(!idx)+1) !inter;
  (!idx, !inter, res)
;;


let affiche_matrice m =
  List.iter
  (
    fun v -> 
      Bitv.iter (fun b -> let s = if b then "1 " else "0 " in Printf.printf "%s" s) v;
      Printf.printf "\n"
  )
  m
;;

let new_gauss z bigNumbers rows cols = 
  let swap_rows bigNums z t i j =
    let temp1 = z.(i) in 
    z.(i) <- z.(j);
    z.(j) <- temp1;
    let temp2 = t.(i) in 
    t.(i) <- t.(j);
    t.(j) <- temp2;
    let temp3 = bigNums.(i) in 
    bigNums.(i) <- bigNums.(j);
    bigNums.(j) <- temp3
  in

  let t = 
    Array.init (rows-1) 
    (
      fun i -> let b = Bitv.create (rows-1) false in 
               Bitv.set b i true;
               b
    )
  in
  (* actual pivot *)
  for j=0 to min (rows - 1) (cols - 1) do 
    (
    (* for each column *)
    Printf.printf "\nBoucle..\nz\n"; print_matrix z;
    Printf.printf "\nt\n"; print_matrix t;
    let found = ref None in 
    for i=j to (rows-2) do
      if (Option.is_none !found) then
        (
        Printf.printf "finding pivot (with j=%d | i=%d)..\n" j i;
        (*finding pivot*)
        if (Bitv.get z.(i) j) then begin
          found := Some ();
          Printf.printf "Pivot found : %d\n" i;
          (*swap columns*)
          (*i is the pivot index, j is the current column, pivot row must be swaped with j row in 'z'; 't'; 'bigNumbers*)
          swap_rows bigNumbers z t i j
        end
        )
    done;
    (*applying modifications from pivot to other rows lower*)
    let pivot_row_z = z.(j) in 
    let pivot_row_t = t.(j) in
    for i=(j+1) to (rows-2) do 
      z.(i) <- Bitv.bw_xor z.(i) pivot_row_z; 
      t.(i) <- Bitv.bw_xor t.(i) pivot_row_t
    done
    )
  done;
  (z, t)
;;

let pivot_gauss bigList z cols rows =
  let t = 
    let resT = ref [] in
    let i = ref (rows-1) in
    while (!i)>=0 do
      let v = Bitv.create rows false in 
      Bitv.set v !i true;
      resT := v :: !resT;
      i:=!i-1
    done;
    !resT
  in

  let keep_gaussing = ref true in 
  let already_used = ref [] in
  for j=0 to min (rows - 1) (cols - 1) do 
    if !keep_gaussing then
    (
      (*find a pivot*)
      let p = ref (-1) in 

      let continue = ref true in
      let () =
        for k = 0 to (rows - 1) do 
          if (!continue) then

            begin
              if not (List.mem k !already_used) then begin
                let v = List.nth z k in
                if Bitv.get v j then begin
                    already_used := k :: !already_used;
                    p := k;
                    continue := false;
                  end
              end
            end

        done
      in
      
      if (!p>=0) then begin
        (*apply modifications*)
        for k = 0 to (rows - 1) do 
          if (k = !p) then 
            () 
          else begin
            let nthV = List.nth z k in
            let currB = Bitv.get nthV j in
            if currB then begin
              let nthVk = List.nth t k in 
              Bitv.iteri (fun i b -> 
                Bitv.set nthV i (xor b (Bitv.get nthV i))
              ) (List.nth z !p);
              Bitv.iteri (fun i b -> 
                Bitv.set nthVk i (xor b (Bitv.get nthVk i))
              ) (List.nth t !p)
            end else ()
          end
        done

      end;
    )
  done;
  (z, t)
;;

let new_get_matrice_Z_from_list_T list amount_factors diff_factor_list idxHash =
  let bitvNull = Bitv.create 0 false in
  let res = Array.make amount_factors bitvNull in
  let bigRes = Array.make amount_factors (Big_int.zero_big_int) in
  let rows = ref 1 in
  List.iteri
    (
      fun i (big, curr_facteurs_list) ->
        
        let vector = 
          let v = Bitv.create amount_factors false in
          List.iter 
            (
              fun value ->
                let nth = Hashtbl.find idxHash value in
                Bitv.set v nth (xor (Bitv.get v nth) true) 
            ) 
            curr_facteurs_list;
          v 
        in
        res.(i) <- vector;
        bigRes.(i) <- big;
        rows := !rows +1
    )
    list;
  (res, bigRes, rows)
;;

let get_matrice_Z_from_list_T list =
  let (size, idxvalues, idxHash) = create_Z_idx_hashtbl_from_tab_T list in
  let resBig = ref [] in
  let resVector = ref [] in 
  List.iter 
  (
    fun (big , facteurs_list) ->
      let vector = 
        let v = Bitv.create size false in 
        List.iter 
        (
          fun value ->
            let nth = Hashtbl.find idxHash value in
            Bitv.set v nth (xor (Bitv.get v nth) true) 
        ) 
        facteurs_list;
        v
      in
      resBig := big :: !resBig;
      resVector := vector :: !resVector;
  )
  list;
  (!resBig, idxvalues, !resVector)
;;

let gcd_answer n x y =
  let gcd a b =
    Big_int.gcd_big_int a b
  in
  gcd (Big_int.sub_big_int x y) n
;;

let get_gcd_from_z_t z t bigNums rows hash_idx_values n = 
    let ans = ref None in
    for i=0 to (rows-1) do
      if (Option.is_none !ans) then
        if (Bitv.all_zeros z.(i)) then 
          (*found an answer*)
          let all_rows_needed = ref [] in 
          Bitv.iteri (fun i b -> if b then all_rows_needed:= i :: !all_rows_needed) t.(i);
          
          (*calculating X and Y -needed for gcd- *)
          let x = ref (Big_int.unit_big_int) in 
          let y = ref (Big_int.unit_big_int) in 

          List.iter
          (
            fun idx ->
              let curr_big = bigNums.(idx) in 
              let curr_vect_z = z.(idx) in 
              x := Big_int.mult_big_int (!x) curr_big;
              Bitv.iteri (fun indice b -> if b then y:= Big_int.mult_int_big_int (Hashtbl.find hash_idx_values indice) (!y)) curr_vect_z;
          ) 
          !all_rows_needed;
          y:= Big_int.sqrt_big_int (!y);
          (*if gcd isn't 1 then answer*)
          let gcd_answer_var = (gcd_answer n !x !y) in
          if (not (Big_int.eq_big_int gcd_answer_var (Big_int.unit_big_int))) then 
            ans:=Some gcd_answer_var
    done;
    !ans
;;

let resolution_from_file_T n b tab amount_factors diff_factor_list hash =
  Printf.printf ("Step 1..\n");
  let (matriceZ, bigNumbers, rows) = new_get_matrice_Z_from_list_T tab amount_factors diff_factor_list hash in
  Printf.printf ("Step 2..\n");
  let (z,t) = new_gauss matriceZ bigNumbers !rows amount_factors in
  Printf.printf ("Printing z and t :\n");
  Printf.printf "z\n"; print_matrix z;
  Printf.printf "\nt\n"; print_matrix t;
  Printf.printf ("Step 3..\n");
  let gcd = get_gcd_from_z_t z t bigNumbers !rows hash n in 
  (* using GCD to find an answer *)
  Printf.printf ("Step 4..\n");
  match (gcd) with
  | None -> failwith "shouldn't happen (end of resultion)"
  | Some gcd ->
      Printf.printf "%s = %s * %s\n" (Big_int.string_of_big_int n) (Big_int.string_of_big_int (Big_int.div_big_int n gcd)) (Big_int.string_of_big_int gcd);
;;