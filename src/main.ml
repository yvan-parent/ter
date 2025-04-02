let module_name = ref "Bitv"
let file_name = ref "T20"
let opti = ref "T"
let time = ref "F"
let speclist =  
  [ ("-mod", Arg.Set_string module_name, "Module_name");
    ("-file", Arg.Set_string file_name, "File_name");
    ("-opti", Arg.Set_string opti, "Optimized");
    ("-time", Arg.Set_string time, "Time_showed") ]
let usage_msg = "Usage: -mod <module_name> -file <file_name> -opti <T/F> -time <T/F>";;
Arg.parse speclist (fun _ -> ()) usage_msg;;

let is_opti =
  match (!opti) with 
  | "F" -> false
  | "T" -> true
  | _ -> failwith "-opti has a wrong input value"
in

let timeShowed =
  match (!time) with 
  | "F" -> None
  | "T" -> Some ()
  | _ -> failwith "-time has a wrong input value"
in
 
match (!module_name) with 
  | "Bitv" -> 
      let module M = Utilities.MakeQuadraticSieve(Types.BitvImpl) in 
      M.full_resolution !file_name is_opti timeShowed
  | "RefArray" ->
      let module M = Utilities.MakeQuadraticSieve(Types.ArrayImpl) in 
      M.full_resolution !file_name is_opti timeShowed
  | "BatBitSet" ->
      let module M = Utilities.MakeQuadraticSieve(Types.BatBitSetImpl) in 
      M.full_resolution !file_name is_opti timeShowed
  | "Zarith" ->
      let module M = Utilities.MakeQuadraticSieve(Types.ZarithImpl) in 
      M.full_resolution !file_name is_opti timeShowed
  | _ -> 
      failwith "Module name not recognized"