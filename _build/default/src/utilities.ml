let xor a b =
  (a || b) && not (a && b)
;;

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
                              let num_list = 
                                match String.split_on_char ' ' r with
                                | [] -> []
                                | _ :: ll -> List.map int_of_string ll
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
  (!n, !b, !tab)
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

  (*algo de gauss*)

  (*
  Printf.printf "\nAffichage base : \n";
    affiche_matrice z;
    Printf.printf "\n";
    affiche_matrice t;
    *)
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

let resolution_from_file_T n b tab =
  let (bigList, idxvalues, vectorList) = get_matrice_Z_from_list_T tab in
  let idxvaluesLen = List.length idxvalues in
  let vectorListLen = List.length vectorList in
  let (z, t) = pivot_gauss bigList vectorList (idxvaluesLen) (vectorListLen) in

  let x = ref (Big_int.unit_big_int) in 
  let y = ref (Big_int.unit_big_int) in  
  
  (* Check for lines in z full of 0*)
  let check_for_fullZero_line_solution z t =

    let indices_to_x_y indices =
      let hash_num_occurence = Hashtbl.create idxvaluesLen in
      List.iter 
      (fun i -> 
        let currBig = (List.nth bigList i) in
        x:= Big_int.mult_big_int (!x) currBig;
        (*adding y numbers (only one copy of each double X to avoid doing a sqrt after)*)
        let (_, tab_elem) =
          List.find (fun (b,l) -> Big_int.eq_big_int b currBig) tab 
        in
        List.iter (fun idx -> 
          match Hashtbl.find_opt hash_num_occurence idx with
          | None ->  y:= (Big_int.mult_int_big_int idx !y); Hashtbl.replace hash_num_occurence idx ()
          | Some _ -> Hashtbl.remove hash_num_occurence idx
          ) tab_elem
      ) 
      indices;
      x:= Big_int.mod_big_int (!x) n;
      y:= Big_int.mod_big_int (!y) n
    in

    let rec aux line =
      if line>=vectorListLen then None 
      else
        begin
          let res = ref [] in
          let current_vec = List.nth z line in  
          let is_answer = Bitv.fold_left (fun acc b -> (not b) && acc) true current_vec in 
          if (is_answer) then
            let t_vec = List.nth t line in 
            Bitv.iteri (fun idx b-> if b then res:= !res@[idx]) t_vec;
            indices_to_x_y (!res);
            (*if gcd isn't 1 then answer*)
            let gcd_answer_var = (gcd_answer n !x !y) in
            if (Big_int.eq_big_int gcd_answer_var (Big_int.unit_big_int)) then
                aux (line+1)
            else 
              Some gcd_answer_var
          else
            aux (line+1)
        end
    in
    aux 0
  in

  (* using GCD to find an answer *)

  match (check_for_fullZero_line_solution z t) with
  | None -> failwith "shouldn't happen (end of resultion)"
  | Some gcd ->
      Printf.printf "%s = %s * %s\n" (Big_int.string_of_big_int n) (Big_int.string_of_big_int (Big_int.div_big_int n gcd)) (Big_int.string_of_big_int gcd) 
;;