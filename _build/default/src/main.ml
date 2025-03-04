let () =
  Printf.printf "Enter a file name: \n%!";
  let input = read_line () in
  let (n, b, tab, amount_factors, diff_factor_list, hash, rows) = Utilities.read_file_T ("src/test/"^input^".txt") in
  Utilities.resolution_from_file_T n b tab amount_factors diff_factor_list hash rows (Some ())
