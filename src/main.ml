let module_name = ref "Bitv"
let file_name = ref "T20"
let opti = ref "True"
let speclist =  
  [ ("-m", Arg.Set_string module_name, "Module_name");
    ("-f", Arg.Set_string file_name, "File_name");
    ("-o", Arg.Set_string opti, "Optimized") ]
let usage_msg = "Usage: -m <module_name> -f <file_name> -o <True/False";;
Arg.parse speclist (fun _ -> ()) usage_msg;;

let is_opti =
  match (!opti) with 
  | "False" -> false
  | "True" -> true
  | _ -> failwith "-o has a wrong input value"
in
 
match (!module_name) with 
  | "Bitv" -> 
      let module M = Utilities.MakeQuadraticSieve(Types.BitvImpl) in 
      M.full_resolution !file_name is_opti
  | "RefArray" ->
      let module M = Utilities.MakeQuadraticSieve(Types.ArrayImpl) in 
      M.full_resolution !file_name is_opti
  | "BatBitSet" ->
      let module M = Utilities.MakeQuadraticSieve(Types.BatBitSetImpl) in 
      M.full_resolution !file_name is_opti
  | "Zarith" ->
      let module M = Utilities.MakeQuadraticSieve(Types.ZarithImpl) in 
      M.full_resolution !file_name is_opti
  | _ -> 
      failwith "Module name not recognized"