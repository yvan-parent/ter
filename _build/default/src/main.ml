let () =
  let (_, b, _) = Utilities.read_file_T "src/test/T30.txt" in
  Printf.printf "num: %d\n" b