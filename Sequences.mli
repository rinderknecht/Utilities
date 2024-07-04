(* nseq:    non-empty sequence;
   sepseq:  (possibly empty) sequence of separated items;
   nsepseq: non-empty sequence of separated items. *)

type           'a nseq = 'a * 'a list
type ('a,'sep) nsepseq = 'a * ('sep * 'a) list
type ('a,'sep)  sepseq = ('a,'sep) nsepseq option

type ('a,'sep) nsep_or_term = [
  `Sep  of ('a,'sep) nsepseq
| `Term of ('a * 'sep) nseq
]

type ('a,'sep) sep_or_term = ('a,'sep) nsep_or_term option

type ('a,'sep) nsep_or_pref = [
  `Sep  of ('a,'sep) nsepseq
| `Pref of ('sep * 'a) nseq
]

(* Creation *)

val nseq_create    : 'a ->          'a list ->           'a nseq
val nsepseq_create : 'a -> ('sep * 'a) list -> ('a,'sep) nsepseq
val sepseq_create  : 'a -> ('sep * 'a) list -> ('a,'sep)  sepseq

(* Singletons *)

val nseq_singleton    : 'a -> 'a nseq
val nsepseq_singleton : 'a -> ('a,'sep) nsepseq
val sepseq_singleton  : 'a -> ('a,'sep) sepseq

(* Consing *)

val nseq_cons    : 'a -> 'a nseq -> 'a nseq
val nsepseq_cons : 'a -> 'sep -> ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_cons  : 'a -> 'sep -> ('a,'sep)  sepseq -> ('a,'sep) nsepseq

val nsep_or_term_cons :
  'a -> 'sep -> ('a,'sep) nsep_or_term -> ('a,'sep) nsep_or_term

val sep_or_term_cons :
  'a -> 'sep -> ('a,'sep) sep_or_term -> ('a,'sep) nsep_or_term

val nsep_or_pref_cons :
  'a -> 'sep -> ('a,'sep) nsep_or_pref -> ('a,'sep) nsep_or_pref

(* Reversing *)

val nseq_rev    : 'a nseq -> 'a nseq
val nsepseq_rev : ('a,'sep) nsepseq -> ('a,'sep) nsepseq
val sepseq_rev  : ('a,'sep)  sepseq -> ('a,'sep)  sepseq

val nsep_or_term_rev : ('a,'sep) nsep_or_term -> ('a,'sep) nsep_or_term
val sep_or_term_rev  : ('a,'sep)  sep_or_term -> ('a,'sep)  sep_or_term
val nsep_or_pref_rev : ('a,'sep) nsep_or_pref -> ('a,'sep) nsep_or_pref


(* Rightwards iterators *)

val nseq_foldl    : ('a -> 'b -> 'a) -> 'a ->           'b nseq -> 'a
val nsepseq_foldl : ('a -> 'b -> 'a) -> 'a -> ('b,'sep) nsepseq -> 'a
val sepseq_foldl  : ('a -> 'b -> 'a) -> 'a -> ('b,'sep)  sepseq -> 'a

val nsep_or_term_foldl :
  ('a -> 'b -> 'a) -> 'a -> ('b,'sep) nsep_or_term -> 'a

val sep_or_term_foldl :
  ('a -> 'b -> 'a) -> 'a -> ('b,'sep) sep_or_term -> 'a

val nsep_or_pref_foldl :
  ('a -> 'b -> 'a) -> 'a -> ('b,'sep) nsep_or_pref -> 'a

val nseq_iter    : ('a -> unit) ->           'a nseq -> unit
val nsepseq_iter : ('a -> unit) -> ('a,'sep) nsepseq -> unit
val sepseq_iter  : ('a -> unit) -> ('a,'sep)  sepseq -> unit

(* Leftwards iterators *)

val nseq_foldr    : ('a -> 'b -> 'b) ->           'a nseq -> 'b -> 'b
val nsepseq_foldr : ('a -> 'b -> 'b) -> ('a,'sep) nsepseq -> 'b -> 'b
val sepseq_foldr  : ('a -> 'b -> 'b) -> ('a,'sep)  sepseq -> 'b -> 'b

val nsep_or_term_foldr : ('a -> 'b -> 'b) -> ('a,'sep) nsep_or_term -> 'b -> 'b
val sep_or_term_foldr  : ('a -> 'b -> 'b) -> ('a,'sep)  sep_or_term -> 'b -> 'b
val nsep_or_pref_foldr : ('a -> 'b -> 'b) -> ('a,'sep) nsep_or_pref -> 'b -> 'b

val nsep_or_term_iter : ('a -> unit) -> ('a,'sep) nsep_or_term -> unit
val sep_or_term_iter  : ('a -> unit) -> ('a,'sep)  sep_or_term -> unit
val nsep_or_pref_iter : ('a -> unit) -> ('a,'sep) nsep_or_pref -> unit

(* Maps *)

val nseq_map    : ('a -> 'b) -> 'a nseq -> 'b nseq
val nsepseq_map : ('a -> 'b) -> ('a,'sep) nsepseq -> ('b,'sep) nsepseq
val sepseq_map  : ('a -> 'b) -> ('a,'sep)  sepseq -> ('b,'sep)  sepseq

val nsep_or_term_map :
  ('a -> 'b) -> ('a,'sep) nsep_or_term -> ('b,'sep) nsep_or_term

val sep_or_term_map  :
  ('a -> 'b) -> ('a,'sep)  sep_or_term -> ('b,'sep) sep_or_term

val nsep_or_pref_map :
  ('a -> 'b) -> ('a,'sep) nsep_or_pref -> ('b,'sep) nsep_or_pref

(* Conversions to non-empty lists *)

val nsepseq_to_nseq      : ('a,'sep) nsepseq      -> 'a nseq
val nsep_or_term_to_nseq : ('a,'sep) nsep_or_term -> 'a nseq
val nsep_or_pref_to_nseq : ('a,'sep) nsep_or_pref -> 'a nseq

val nseq_to_nsepseq : 'sep -> 'a nseq -> ('a,'sep) nsepseq

(* Conversions to lists *)

val nseq_to_list    :        'a nseq -> 'a list
val nsepseq_to_list : ('a,'b) nsepseq -> 'a list
val sepseq_to_list  : ('a,'b)  sepseq -> 'a list

val nsep_or_term_to_list : ('a,'sep) nsep_or_term -> 'a list
val sep_or_term_to_list  : ('a,'sep)  sep_or_term -> 'a list
val nsep_or_pref_to_list : ('a,'sep) nsep_or_pref -> 'a list

(* Map and concatenate lists *)

val nseq_concat_map    : ('a -> 'b list) ->           'a nseq -> 'b list
val nsepseq_concat_map : ('a -> 'b list) -> ('a,'sep) nsepseq -> 'b list
val sepseq_concat_map  : ('a -> 'b list) -> ('a,'sep)  sepseq -> 'b list

(* Conversions from lists *)

val sep_or_term_of_list:
  'sep ->
  [< `Sep | `Term ] ->
  'a list ->
  ('a,'sep) sep_or_term

val list_to_sepseq : 'a list -> 's -> ('a, 's) sepseq

(* Conversions to JSON *)

type json = Yojson.Safe.t

val yojson_of_nseq : ('a -> json) -> 'a nseq -> json

val yojson_of_nsepseq :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) nsepseq -> json

val yojson_of_sepseq :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) sepseq -> json

val yojson_of_sep_or_term :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) sep_or_term -> json

val yojson_of_nsep_or_term :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) nsep_or_term -> json

val yojson_of_nsep_or_pref :
  ('a -> json) -> ('sep -> json) -> ('a,'sep) nsep_or_pref -> json
