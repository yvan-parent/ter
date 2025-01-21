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
                              let rec aux liste =
                                match liste with
                                | [] -> []
                                | e :: ll -> int_of_string e :: (aux ll)
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

let remove_doublons_X_vector bigList vectorList =
  let rec reduction_doublon bigList vectorList acc =
    match bigList, vectorList with
    | big :: l1, v :: l2 -> 
      if (List.mem v acc) then 
        reduction_doublon l1 l2 acc
      else
        let bb, vv = (reduction_doublon l1 l2 (v::acc)) in 
        (big :: bb, v :: vv) 
    | [], [] -> ([], [])
    | _ -> failwith ("bigList and vectorList somehow don't have the same size")
  in
  reduction_doublon bigList vectorList []
;;

let pivot_gausse bigList vectorList =
  let bL_reduced, vL_reduced = remove_doublons_X_vector bigList vectorList in 
  (bL_reduced, vL_reduced)
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

(*Affichage du resultat de la fonction 'get_matrice_Z_from_list_T list'*)
let print_matrice tab =
  let (bigList, idxvalues, vectorList) = get_matrice_Z_from_list_T tab in
  let affichage bigList vectorList =
    List.iteri 
      (
        fun i b -> 
          begin
            Printf.printf "%s : " (Big_int.string_of_big_int b);
            let v = (List.nth vectorList i) in 
            Bitv.iter (fun b -> if b then Printf.printf "1 " else Printf.printf"0 ") v ;
            Printf.printf "\n"
          end
      ) 
    bigList;
  in 
  List.iter (fun i -> Printf.printf "%d " i) idxvalues;
  Printf.printf "\n\n";
  affichage bigList vectorList;
  Printf.printf "\nResultat apres supression des doublons :\n";
  let bL_reduced, vL_reduced = remove_doublons_X_vector bigList vectorList in
  affichage bL_reduced vL_reduced
;;

let resolution_from_file_T n b tab =
  (*let (bigList, vectorList) = get_matrice_Z_from_list_T tab in*)
  failwith "pivot todo"
;;