module type RowType = sig
  type t

  val xor : t -> t -> t
  val create : int -> t
  val empty : t
  val set : t -> int -> bool -> unit
  val get : t -> int -> bool
  val all_zeros : t -> bool
  val iteri : (int -> bool -> unit) -> t -> unit
end


module BitvImpl : RowType = struct 
  type t = Bitv.t
  let xor t1 t2 = Bitv.bw_xor t1 t2
  let create i = Bitv.create i false
  let empty = create 0
  let set t i b = Bitv.set t i b
  let get t i = Bitv.get t i 
  let all_zeros t = Bitv.all_zeros t
  let iteri = Bitv.iteri
end

(* Very bad one *)
module ArrayImpl : RowType = struct 
  type t = bool ref array
  let xor t1 t2 = 
    Array.iter2 (fun b1 b2 -> b1 := !b1 <> !b2) t1 t2;
    t1
  let create i = Array.init i (fun _ -> ref false)
  let empty = create 0
  let set t i b = Array.set t i (ref b)
  let get t i = !(Array.get t i)
  let all_zeros t = Array.for_all (fun b -> not !b) t
  let iteri f t = Array.iteri (fun i ref_b -> f i !ref_b) t
end

exception StopAnswer
module BitSetImpl : RowType = struct
  type t = BitSet.t
  let empty = BitSet.empty ()
  let create i = BitSet.create i 
  let set t i b = if b then 
                    BitSet.set t i 
                  else 
                    BitSet.unset t i 
  let get t i = BitSet.is_set t i
  let xor t1 t2 = 
    try
      (
        let i = ref 0 in 
        while (true) do 
          let () =
          (
            if (get t2 !i) then 
              (
                if (get t1 !i) then 
                  BitSet.unset t1 !i 
                else
                  BitSet.set t1 !i
              )
          )
          in 
          i:=!i+1
        done;
        t1
      )
    with _ -> t1
  ;;
  let all_zeros t = 
    try 
      (
        let i = ref 0 in 
        while (true) do 
          if (get t !i) then raise StopAnswer
          else i:=!i+1
        done;
        true
      )
    with 
    | StopAnswer -> false
    | _ -> true
  ;;
  let iteri f t =
    try
      (
        let i = ref 0 in 
        while (true) do 
          let () =
          (f !i (get t !i)) in 
          i:=!i+1
        done;
      )
    with _ -> ()
  ;;
end
