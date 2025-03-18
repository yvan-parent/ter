module type RowType = sig
    type t
  
    val xor : t array -> int -> int -> unit (* do the operation on the first parameter *)
    val create : int -> t
    val empty : t
    val set : t -> int -> bool -> unit
    val get : t -> int -> bool
    val all_zeros : t -> bool
    val idxSet : t -> int list
  end
  
  module BitvImpl : RowType
  module ArrayImpl : RowType
  module BatBitSetImpl : RowType