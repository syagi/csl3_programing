module  type Heap_t = sig

type ('a, 'b) t 

val create : int -> 'a -> 'b -> ('a, 'b) t

val insert : ('a,'b) t -> 'a -> 'b index_t * ('a, 'b) t 

type index_t

val get : ('a, 'b) t -> index_t -> 'a * 'b

val set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t 

val split_top : ('a, 'b) t -> ('a, 'b) * ('a , 'b) t 

val length: ('a, 'b) t -> int 

end
