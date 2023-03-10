(* nseq:    non-empty sequence;
   sepseq:  (possibly empty) sequence of separated items;
   nsepseq: non-empty sequence of separated items. *)

type           'a nseq = 'a * 'a list
type ('a,'sep) nsepseq = 'a * ('sep * 'a) list
type ('a,'sep)  sepseq = ('a,'sep) nsepseq option

(* Consing *)

val nseq_cons    : 'a -> 'a nseq -> 'a nseq
val nsepseq_cons : 'a -> 'sep -> ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_cons  : 'a -> 'sep -> ('a,'sep)  sepseq -> ('a,'sep) nsepseq

(* Reversing *)

val nseq_rev    : 'a nseq -> 'a nseq
val nsepseq_rev : ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_rev  : ('a,'sep)  sepseq -> ('a,'sep)  sepseq

(* Rightwards iterators *)

val nseq_foldl    : ('a -> 'b -> 'a) -> 'a ->        'b nseq -> 'a
val nsepseq_foldl : ('a -> 'b -> 'a) -> 'a -> ('b,'c) nsepseq -> 'a
val sepseq_foldl  : ('a -> 'b -> 'a) -> 'a -> ('b,'c)  sepseq -> 'a

val nseq_iter    : ('a -> unit) ->        'a nseq -> unit
val nsepseq_iter : ('a -> unit) -> ('a,'b) nsepseq -> unit
val sepseq_iter  : ('a -> unit) -> ('a,'b)  sepseq -> unit

(* Leftwards iterators *)

val nseq_foldr    : ('a -> 'b -> 'b) ->        'a nseq -> 'b -> 'b
val nsepseq_foldr : ('a -> 'b -> 'b) -> ('a,'c) nsepseq -> 'b -> 'b
val sepseq_foldr  : ('a -> 'b -> 'b) -> ('a,'c)  sepseq -> 'b -> 'b

(* Maps *)

val nseq_map    : ('a -> 'b) -> 'a nseq -> 'b nseq
val nsepseq_map : ('a -> 'b) -> ('a,'c) nsepseq -> ('b,'c) nsepseq
val sepseq_map  : ('a -> 'b) -> ('a,'c)  sepseq -> ('b,'c)  sepseq

(* Conversions to lists *)

val nseq_to_list    :        'a nseq -> 'a list
val nsepseq_to_list : ('a,'b) nsepseq -> 'a list
val sepseq_to_list  : ('a,'b)  sepseq -> 'a list
