open List;;
exception AlError;;
exception BstError of string;;
(* 1st version
(* is Member *)
(* TODO: if else question *)
let rec isMember e l = 
  if l = []
    then false
  else if (hd l) = e 
    then true
    else isMember e (tl l);;(* tail recursive*)


*)

(* 2nd version
 let isMember e l = 
  let rec Membering l = 
    if l = []
      then false
    else if hd l = c
      then true
    else Membering (tl l)
  in Membering l;; *)

(* 3rd version *)
let isMember e l = 
  let rec membering l = 
    (l <> []) && (hd l = e || membering (tl l))
  in membering l;;

let append l r = 
  let rec appending l = 
    if l = []
      then r
    else (hd l)::(appending (tl l))
  in if r = []
    then l 
  else appending l;;

let reverse u = 
  let rec reversing u r = 
    if u = []
      then r 
    else reversing (tl u) ((hd u) :: r)
  in reversing u [];;

let eps = 0.00001


(* 
(* version 1 *)
let squirt x = 
  let rec squirting g h = 
    if abs_float(g -. h) < eps
      then squirting ((g+h)/.2.0) (x/.((g+h)/.2.0))
  in squirting 1.0 x;; *)


(* version 2 *)
let squirt x = 
  let rec squirting g h = 
    if abs_float(g -. h) < eps
      then g 
    else let g' = (g+.h)/.2.0 in 
          let h' = x/.g' in
      squirting g' h'
  in squirting 1.0 x;;


let birthday = (5,12);;
let month = fst birthday;;
let day = snd birthday;;

(* let alGet l k = 
  let rec alGetting l = 
    if l = []
      then raise AlError 
    else let p = hd l 
          in if fst p = k
            then snd p
            else alGetting (tl l)
  in alGetting l;;
  (* ('a * 'b)list -> 'a -> 'b *)
 *)

 (* 2nd version *)
 let alGet l k = 
  let rec alGetting l = 
    match l 
      with []-> raise AlError |
        (k',v')::l'->
          if k = k'
            then v'
          else alGetting l'
  in alGetting l;;

let alPut l k v = 
  (k,v) :: l;;

(* ('a * 'b)list -> 'a -> 'b -> ('a * 'b) list *)


(* user defiend types *)
(* type must begin with lower case 
   constructor must begin with upper case*)
type al = 
  AlEmpty |
  Al of (int * int * al);;

let temp = Al(1,2,Al(2,4,AlEmpty))

type 'value al = 
  AlEmpty|
  Al of (int * 'value * 'value al);;

  (* most general *)
type ('key,'value) al = 
  AlEmpty|
  Al of ('key * 'value * ('key,'value)al);;

let alGet l k = 
  let rec alGetting l = 
    match l 
    with AlEmpty->raise AlError|
      Al(k',v',l') -> 
        if k = k'
          then v'
        else alGetting l'
  in alGetting l;;


type ('key,'value) bst = 
  BSTEmpty | 
  BSTNode of ('key * 'value * ('key,'value)bst * ('key,'value)bst);;

(* no null subtree, BSTEmpty is a pointer to something... *)

let bstGet root key = 
  let rec bstGetting subtree = 
    match subtree with 
      BSTEmpty -> raise BstError "No such key."|
      BstNode (otherKey, otherValue, left, riht) -> 
        if key < otherKey
          then bstGetting left 
        else if key > otherKey
          then bstGetting right
          else otherValue
  in bstGetting root;;

  (* ('key * 'value)bst -> 'key -> 'value *)

(* persistent data object def1: move it to disk, so it's persistent
   def 2:    original object persists *)
