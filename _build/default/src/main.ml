let () =
  Printf.printf "\n";
  let (n, b, tab) = Utilities.read_file_T "src/test/mini.txt" in
  Utilities.test_get_matrice_Z_from_list_T tab; 
  Utilities.resolution_from_file_T n b tab