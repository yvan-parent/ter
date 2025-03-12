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
  
  module BitvImpl : RowType
  module ArrayImpl : RowType
  module BitSetImpl : RowType