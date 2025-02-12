let () =
  Printf.printf "\nmini:\n";
  let (n, b, tab, amount_factors, diff_factor_list, hash) = Utilities.read_file_T "src/test/mini.txt" in
  Utilities.resolution_from_file_T n b tab amount_factors diff_factor_list hash
