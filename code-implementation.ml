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


type proposition = False | True | Var of string | Not of proposition | And of proposition * proposition| 
                    Or of proposition * proposition | Imply of proposition * proposition | Equiv of proposition * proposition;;

(* data structure of manipulating propositions in ocaml *)
(* Imply (Not (And (Var "p", Var "q")),Or ((Not (Var "p")),Not (Var "q"))) *)

let evaluate proposition pairs = 
  let rec evaluating proposition = 
    match proposition with 
      False -> false | (* False: constructor that constructs proposition; false: literal*)
      True -> true | 
      Var name -> alGet pairs name | 
      Not right -> not (evaluating right)|
      And (left,right) -> (evaluating left) && (evaluating right) |
      Or (left, right) -> (evaluating left) && (evaluating right) | 
      Imply (left, right) -> (not (evaluating left)) || (evaluating right) |
      Equiv (left, right) -> (evaluating left) = (evaluating right)
  in evaluating proposition;;

let generatePairs etc names = 
  let rec generating names pairs = 
    match names with 
      [] -> etc pairs|
      name::otherNames -> 
        generating otherNames (alPut pairs name false);
        generating otherNames (alPut pairs name true)
  in generating names [];;

  let generateAndTestPairs etc names = 
    let rec generating names pairs = 
      match names with 
        [] -> etc pairs|
        name::otherNames -> 
          generating otherNames (alPut pairs name false) && 
          generating otherNames (alPut pairs name true)
    in generating names [];;


let names proposition = 
  let namesing proposition = match proposition with
    False | True -> []|
    Var name -> [name]|
    Not right -> namesing right|
    And (left,right) -> namesing left @ namesing right  (* @: appened operator: sticks two lists together*)
  in uniquify (names proposition);;

let isTautology proposition = 
  generateAndTestPairs (fun pairs -> evaluate proposition pairs) (names proposition);;

(* search based programming / brute-force algorithm *)

isTautology (And (Var "p",Var "q"));;




(* And Var x, y*)

(* streams: ordered sequence of object that appears to be infinitely long 
   but only a finite part actually exists*)


(* stream definition: 
   stream1: data structure for IO
   stream2: infinite series in finite space *)
  
   let makeStream this state next = 
    ((this, state), next);;
  
  let first ((this,_),_)= this;;
  
  let rest ((this,state),next) = 
    (next this state, next);;
  
    (* unit object *)
  let naturals = makeStream 0 () (fun this tate -> ((this+1),state));; (* ((0,()),<fun>)*)
  
  let temp = first rest naturals;;
  
  let factorials = makeStream 1 1 (fun this state -> ((this*state),state+1));;

  let temp = first rest rest factorials;;

  let rec take count stream = 
    match count with 
      0 -> stream | 
      _ -> take (count-1) (rest stream);;

let temp = first (take 4 factorials);;
let rec takeList count stream = 
  match count with 
    0 -> []|
    _ -> (first stream)::(takeList (count-1) (rest stream));;

(* ()::[1;2;3] *)

let advance predicate stream = 
  let advancing stream = 
    if predicate (first stream) then
      stream 
    else advancing (rest stream) (*if predicate is never satisfied, then advance will hang*)
  in advancing stream;;

let compare straem1 stream2 = 
  makeStream ((frist stream1)=(first stream2)) 
              (((rest stream1),(rest stream2))) 
              (fun _ (s1,s2)->
                  (((frist stream1)=(first stream2)),
                  (((rest stream1),(rest stream2)))))



(* Can we simulate OOP using a functional applicatiev language? 
   OCaml has OOP - it doesn't work that we'll see here. 
   All we've got is functions (closures) -> we can do OOP *)

type 'base stackOperation= 
                IsEmpty|
                Peek|
                Pop|
                Push of 'base;;

type 'base stackResult = 
                BoolResult of bool|
                StackResult of stack|
                BaseResult of 'base;;


