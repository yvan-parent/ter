module type RowType = sig
  type t

  val xor : t -> t -> unit
  val create : int -> t
  val empty : t
  val set : t -> int -> bool -> unit
  val get : t -> int -> bool
  val all_zeros : t -> bool
  val idxSet : t -> int list
end


module BitvImpl : RowType = struct 
  type t = Bitv.t ref
  let create i = ref (Bitv.create i false)
  let empty = create 0
  let set t i b = Bitv.unsafe_set !t i b
  let xor t1 t2 = 
    t1 := Bitv.bw_xor !t1 !t2
  let get t i = Bitv.unsafe_get !t i 
  let all_zeros t = Bitv.all_zeros !t
  let idxSet t =
    let all_rows_needed = ref [] in 
    Bitv.iteri (fun i b -> if b then all_rows_needed:= i :: !all_rows_needed) !t;
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
  let xor t1 t2 = 
    Array.iteri (fun idx b ->  t1.(idx) <- b <> t1.(idx) ) t2;
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
  let xor t1 t2 = 
    BatBitSet.differentiate_sym t1 t2
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

module ZarithImpl : RowType = struct
  type t = Z.t ref
  let empty = ref Z.zero
  let create i = ref Z.zero
  let get t i = Z.testbit !t i
  let set t i b =
    if b then
      let inter = Z.shift_left Z.one i in
      t := Z.logor !t inter
    else
      let inter = Z.shift_left Z.one i in
      let inter = Z.lognot inter in
      t := Z.logand !t inter
  let xor t1 t2 =
    t1 := Z.logxor !t1 !t2
  let all_zeros t = Z.equal !t Z.zero
  let idxSet t =
    let rec aux acc i =
      if i < 0 then 
        acc
      else if Z.testbit !t i then 
        aux (i :: acc) (i - 1)
      else 
        aux acc (i - 1)
    in
    let len = (Z.numbits !t) - 1 in
    aux [] len
end