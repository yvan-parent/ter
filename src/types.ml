module type RowType = sig
  type t

  val xor : t array -> int -> int -> unit
  val create : int -> t
  val empty : t
  val set : t -> int -> bool -> unit
  val get : t -> int -> bool
  val all_zeros : t -> bool
  val idxSet : t -> int list
end


module BitvImpl : RowType = struct 
  type t = Bitv.t
  let create i = Bitv.create i false
  let empty = create 0
  let set t i b = Bitv.unsafe_set t i b
  let xor matr i j = 
    matr.(i) <- Bitv.bw_xor (matr.(i)) (matr.(j))
  let get t i = Bitv.unsafe_get t i 
  let all_zeros t = Bitv.all_zeros t
  let iteri f t = Bitv.iteri f t
  let idxSet t =
    let all_rows_needed = ref [] in 
    iteri (fun i b -> if b then all_rows_needed:= i :: !all_rows_needed) t;
    !all_rows_needed        
end

module ArrayImpl : RowType = struct
  type t = bool array
  let create i = Array.init i (fun _ -> false)
  let empty = create 0
  let set t i b = Array.set t i b
  let get t i = Array.get t i
  let all_zeros t = Array.for_all (fun b -> not b) t
  let iteri f t = Array.iteri (fun i b -> f i b) t
  let xor matr i j = 
    Array.iteri (fun idx b ->  matr.(i).(idx) <- b <> matr.(i).(idx) ) matr.(j);
  ;;
  let idxSet t =
    let res = ref [] in 
    iteri (fun i b -> if b then res:= i::!res) t;
    !res
end

module BatBitSetImpl : RowType = struct
  type t = BatBitSet.t
  let empty = BatBitSet.empty ()
  let create i = BatBitSet.create i
  let set t i b = if b then BatBitSet.set t i else BatBitSet.unset t i 
  let get t i = BatBitSet.mem t i
  let all_zeros t = 
    match (BatBitSet.next_set_bit t 0) with
    None -> true
    | _ -> false
  ;;
  let xor matr i j = 
    BatBitSet.differentiate_sym matr.(i) matr.(j)
  ;;

  let idxSet t =
    let res = ref [] in 
    let rec loop k =
      match (BatBitSet.next_set_bit t k) with 
      | None -> ()
      | Some idx -> res:= idx::!res; loop (idx+1)
    in
    loop 0;
    !res
end