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
      BSTEmpty -> raise (BstError "No such key.")|
      BSTNode (otherKey, otherValue, left, right) -> 
        if key < otherKey
          then bstGetting left 
        else if key > otherKey
          then bstGetting right
          else otherValue
  in bstGetting root;;

  (* ('key * 'value)bst -> 'key -> 'value *)

(* persistent data object def1: move it to disk, so it's persistent
   def 2:    original object persists *)
let bstPut root key value= 
  let rec bstPutting subtree = 
    match subtree with
      BSTEmpty -> BSTNode(key,value,BSTEmpty,BSTEmpty)|
      BSTNode (otherKey,otherValue,left,right)->
        if key < otherKey
          then BSTNode(otherKey,otherValue,(bstPutting left),right) (* This is not tail recursive*)
        else if key > otherKey
          then BSTNode(otherKey,otherValue, left, (bstPutting right)) (* This is not tail recursive*)
        else if value = otherValue
              then subtree
            else BSTNode(key, value, left, right)
  in bstPutting root;;

(* building a BST *)
(* let t = BSTEmpty;;
let t = bstPut t 10 "ten";;
let t = bstPut t 7 "seven";;
let t = bstPut t 7 "seven";;
let t = bstPut t 15 "fifteen";; *)

(* let t = BSTEmpty in 
  let t = bstPut t 10 "ten" in 
    let t = bstPut t 7 "seven" in 
      let t = bstPut t 15 "fifteen" ;; *)

let rec isBstEqual left right = 
  match (left,right) with 
    (BSTEmpty,BSTEmpty) -> true|
    (BSTEmpty,BSTNode (_,_,_,_)) |(BSTNode (_,_,_,_), BSTEmpty) -> false | 
    (BSTNode (l_k,_,left_left,left_right),BSTNode (r_k,_,right_left, right_right)) -> 
      if l_k <> r_k && 
        (isBstEqual left_left right_left) &&  (* not tail recursive*)
        (isBstEqual left_right right_right) then true (* tail recursive*)
      else false;; 

        
let k = 1;;
let f() = k;;
let k = 2 in f();; (* return 1*)

(* function close themselves in their defining environment *)
(* environment associate names with values. "binds" names to values *)
(* function is closed -> remmebers its defining environment (static binding/lexical binding)*)

(* dynamic binding: looks in current environment instead of defining environment  *)

let rest = 2;;
let plusRest n = n+rest;; (* closed in defining environment *)
(* Function line plusRest is represented as an objected called a closure *)
(* closure: 
   [     ] environment
   [     ] parameters 
   [     ] body (code)*)

(* 
  plusRest: 
   [     ] environment    <- rest = 2; (+) = [code??]; 
   [     ] parameters     <- n
   [     ] body (code)    <- n + rest
   *)

   (* (+) 2 3 -> 5
      ( * ) 2 3 -> 6 *)


(* Partial applications / "curried" functions 
   Combinatory logic *)

let add n m = n+m;;
let add_two = add 2;;

(* logic: 
   0-order logic: no predicates 
   1-order logic: predicates that can be applied to objects other than predcates P(x)
   2-order logic: predicates that can be applied to other predicates R(P,Q) ??????????????? *)



let map func objects = 
  let rec mapping objects = 
    match objects with 
      [] -> [] | 
      first::rest -> 
        (func first) :: (mapping rest)
  in mapping objects;;
  (* ('a -> 'b) -> 'a list -> 'b list*) 

let temp = (fun n -> n+1);;
let temp = map (fun n -> n+1) [1;2;3];;

let filter isKept things = 
  let rec filtering things = 
    match things with 
      [] -> []|
      firstThing::otherThings -> 
        if isKept firstThing then
          firstThing::(filtering otherThings)
      else filtering otherThings
  in filtering things;;

let temp = filter (fun n -> n mod 2 = 1) [0;1;2;3;4;5];;

let isOdd n = (n mod 2 = 1);;
let opposite hasProperty = (fun things -> not (hasProperty things));;


(* Continuation passing style (CPS) *)
(* function that returns 0,1,2,3,... values  *)
(* generate values 1 at a time *)

(* In CPS, a functino doesn't return a useful value (It can)
   but calls another function ---- called its continuation *)


let generateBools etc n = 
  let rec generating bools n = 
    match n with 
      0 -> etc bools | 
      _ -> 
        generating (false::bools) (n-1);
        generating (true::bools) (n-1)
  in generating [] n
