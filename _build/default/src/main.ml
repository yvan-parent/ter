let module_name = ref "Bitv"
let file_name = ref "T20";;
let speclist =  
  [ ("-m", Arg.Set_string module_name, "Module_name");
    ("-f", Arg.Set_string file_name, "File_name") ]
let usage_msg = "Usage: -m <module_name> -f <file_name>";;
Arg.parse speclist (fun _ -> ()) usage_msg;;

match (!module_name) with 
  | "Bitv" -> 
      let module M = Utilities.MakeQuadraticSieve(Types.BitvImpl) in 
      M.full_resolution !file_name
  | "RefArray" ->
      let module M = Utilities.MakeQuadraticSieve(Types.ArrayImpl) in 
      M.full_resolution !file_name
  | "BatBitSet" ->
      let module M = Utilities.MakeQuadraticSieve(Types.BatBitSetImpl) in 
      M.full_resolution !file_name
  | "Zarith" ->
      let module M = Utilities.MakeQuadraticSieve(Types.ZarithImpl) in 
      M.full_resolution !file_name
  | _ -> 
      failwith "Module name not recognized"