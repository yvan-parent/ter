let () =
  Printf.printf "\nT20:\n";
  let (n, b, tab) = Utilities.read_file_T "src/test/T20.txt" in
  Utilities.resolution_from_file_T n b tab;
  
  Printf.printf "\nT30:\n";
  let (n, b, tab) = Utilities.read_file_T "src/test/T30.txt" in
  Utilities.resolution_from_file_T n b tab