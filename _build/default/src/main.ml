let () =
  Printf.printf "\nT40:\n";
  let (n, b, tab) = Utilities.read_file_T "src/test/T40.txt" in
  Utilities.resolution_from_file_T n b tab;
  
 