[@@@ocaml.warnings "-32"] (* "unused-value-declaration" --- But how? *)

type        'a    nseq = 'a * 'a list
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

(* Utilities *)

let (<@) f g x = f (g x)

(* Creation *)

let nseq_create    x l = x, l
let nsepseq_create x l = x, l
let sepseq_create  x l = Some (x, l)

(* Singletons *)

let nseq_singleton    x = nseq_create    x []
let nsepseq_singleton x = nsepseq_create x []
let sepseq_singleton  x = sepseq_create  x []

(* Consing *)

let nseq_cons x (hd,tl) = x, hd::tl
let nsepseq_cons x sep (hd,tl) = x, (sep,hd)::tl

let sepseq_cons x sep = function
          None -> x, []
| Some (hd,tl) -> x, (sep,hd)::tl

let nsep_or_term_cons x sep = function
  `Sep  s -> `Sep (nsepseq_cons x sep s)
| `Term s -> `Term (nseq_cons (x,sep) s)

let sep_or_term_cons x sep = function
  None   -> `Term (nseq_singleton (x,sep))
| Some s -> nsep_or_term_cons x sep s

let nsep_or_pref_cons x sep = function
  `Sep  s -> `Sep (nsepseq_cons x sep s)
| `Pref s -> `Pref (nseq_cons (sep,x) s)

(* Rightwards iterators *)

let nseq_foldl f init (hd,tl) = List.fold_left f init (hd::tl)

let nsepseq_foldl f init (hd,tl) =
  List.fold_left (fun a (_,e) -> f a e) (f init hd) tl

let sepseq_foldl f init = function
    None -> init
| Some s -> nsepseq_foldl f init s

let nsep_or_term_foldl f init = function
  `Sep  s -> nsepseq_foldl f init s
| `Term s -> nseq_foldl (fun a -> f a <@ fst) init s

let sep_or_term_foldl f init = function
  None   -> init
| Some s -> nsep_or_term_foldl f init s

let nsep_or_pref_foldl f init = function
  `Sep  s -> nsepseq_foldl f init s
| `Pref s -> nseq_foldl (fun a -> f a <@ snd) init s


let nseq_iter f (hd,tl) = List.iter f (hd::tl)

let nsepseq_iter f (hd,tl) = f hd; List.iter (f <@ snd) tl

let sepseq_iter f = function
    None -> ()
| Some s -> nsepseq_iter f s

let nsep_or_term_iter f = function
  `Sep  s -> nsepseq_iter f s
| `Term s -> nseq_iter (f <@ fst) s

let sep_or_term_iter f = function
  None   -> ()
| Some s -> nsep_or_term_iter f s

let nsep_or_pref_iter f = function
  `Sep  s -> nsepseq_iter f s
| `Pref s -> nseq_iter (f <@ snd) s

(* Reversing *)

let nseq_rev (hd,tl) =
  let rec aux acc = function
      [] -> acc
  | x::l -> aux (nseq_cons x acc) l
in aux (hd,[]) tl

let nsepseq_rev =
  let rec aux acc = function
    hd, (sep,snd)::tl -> aux ((sep,hd)::acc) (snd,tl)
  | hd,            [] -> hd, acc in
function
  hd, (sep,snd)::tl -> aux [sep,hd] (snd,tl)
|                 s -> s

let sepseq_rev = function
      None -> None
| Some seq -> Some (nsepseq_rev seq)

let nsep_or_term_rev = function
  `Sep  s -> `Sep (nsepseq_rev s)
| `Term s -> `Term (nseq_rev s)

let sep_or_term_rev = function
  None   -> None
| Some s -> Some (nsep_or_term_rev s)

let nsep_or_pref_rev = function
  `Sep  s -> `Sep (nsepseq_rev s)
| `Pref s -> `Pref (nseq_rev s)

(* Leftwards iterators *)

let nseq_foldr f (hd,tl) = List.fold_right f (hd::tl)

let nsepseq_foldr f (hd,tl) init =
  f hd (List.fold_right (f <@ snd) tl init)

let sepseq_foldr f = function
    None -> fun init -> init
| Some s -> nsepseq_foldr f s

let nsep_or_term_foldr f = function
  `Sep  s -> nsepseq_foldr f s
| `Term s -> fun init -> nseq_foldr (f <@ fst) s init

let sep_or_term_foldr f = function
  None   -> fun i -> i
| Some s -> nsep_or_term_foldr f s

let nsep_or_pref_foldr f = function
  `Sep  s -> nsepseq_foldr f s
| `Pref s -> fun init -> nseq_foldr (f <@ snd) s init

(* Maps *)

let nseq_map f (hd,tl) = f hd, List.map f tl

let nsepseq_map f (hd,tl) =
  f hd, List.map (fun (sep,item) -> (sep, f item)) tl

let sepseq_map f = function
      None -> None
| Some seq -> Some (nsepseq_map f seq)

let nsep_or_term_map f = function
  `Sep  s -> `Sep (nsepseq_map f s)
| `Term s -> `Term (nseq_map (fun (x, term) -> (f x, term)) s)

let sep_or_term_map f = function
  None   -> None
| Some s -> Some (nsep_or_term_map f s)

let nsep_or_pref_map f = function
  `Sep  s -> `Sep (nsepseq_map f s)
| `Pref s -> `Pref (nseq_map (fun (pref, x) -> (pref, f x)) s)

(* Conversions to non-empty lists *)

let nsepseq_to_nseq (x, l) = nseq_create x (List.map snd l)

let nsep_or_term_to_nseq = function
  `Sep  s -> nsepseq_to_nseq s
| `Term s -> nseq_map fst s

let nsep_or_pref_to_nseq = function
  `Sep  s -> nsepseq_to_nseq s
| `Pref s -> nseq_map snd s

let nseq_to_nsepseq sep (hd,tl) =
  nsepseq_create hd @@ List.map (fun x -> (sep,x)) tl

(* Conversions to lists *)

let nseq_to_list (x,y) = x::y

let nsepseq_to_list (x,y) = x :: List.map snd y

let sepseq_to_list = function
    None -> []
| Some s -> nsepseq_to_list s

let nsep_or_term_to_list = function
  `Sep  s -> nsepseq_to_list s
| `Term s -> List.map fst (nseq_to_list s)

let sep_or_term_to_list = function
  None     -> []
| Some seq -> nsep_or_term_to_list seq

let nsep_or_pref_to_list = function
  `Sep s      -> nsepseq_to_list s
| `Pref (x,l) -> List.map snd (x::l)

(* Conversions from lists *)

let list_to_sepseq (lst : 'a list) (sep : 's) : ('a, 's) sepseq =
  match lst with
    [] -> None
  | hd :: tl -> Some (hd, List.map (fun e -> sep, e) tl)

let sep_or_term_of_list sep sep_or_term lst : ('a,'sep) sep_or_term =
  match lst with
    [] -> None
  | x :: l ->
      match sep_or_term with
        `Sep -> Some (`Sep (nseq_to_nsepseq sep (nseq_create x l)))
      | `Term -> let list = List.map (fun x -> (x,sep)) l
                 in Some (`Term (nseq_create (x,sep) list))

(* Conversions to JSON *)

type json = Yojson.Safe.t

let yojson_of_nseq f (s : 'a nseq) =
  `List (nseq_to_list @@ nseq_map f s)

let yojson_of_nsepseq f _ (s : ('a, 'sep) nsepseq) =
  `List (nsepseq_to_list @@ nsepseq_map f s)

let yojson_of_sepseq f _ (s : ('a, 'sep) sepseq) =
  `List (sepseq_to_list @@ sepseq_map f s)

let yojson_of_sep_or_term f _ (s : ('a, 'sep) sep_or_term) =
  `List (sep_or_term_to_list @@ sep_or_term_map f s)

let yojson_of_nsep_or_term f _ (s : ('a, 'sep) nsep_or_term) =
  `List (nsep_or_term_to_list @@ nsep_or_term_map f s)

let yojson_of_nsep_or_pref f _ (s : ('a, 'sep) nsep_or_pref) =
  `List (nsep_or_pref_to_list @@ nsep_or_pref_map f s)

(* Map and concatenate lists *)

let    nseq_concat_map f s = List.concat_map f (nseq_to_list    s)
let  sepseq_concat_map f s = List.concat_map f (sepseq_to_list  s)
let nsepseq_concat_map f s = List.concat_map f (nsepseq_to_list s)
