(*

  TAUTOLOGY. A brute-force tautology checker for propositional logic.

    James Moen
    16 Feb 24

  This is the tautology checker that was discussed in the last few lectures. It
  also includes some code (mostly for printing) that was not discussed.

*)

open List ;;    (* Functions that operate on lists. *)
open Printf ;;  (* Functions that print objects. *)

(* AL. An association list: a list of 2-tuples that associate KEY's with their
   corresponding VALUE's. *)

type ('key, 'value) al = ('key * 'value) list ;;

(* AL GET. Return the value of KEY from the association list PAIRS. Raise the
   exception NO SUCH KEY if KEY is not in PAIRS. *)

exception NoSuchKey ;;

let alGet pairs key =
  let rec alGetting pairs =
    match pairs
    with [] ->
           raise NoSuchKey |
         (otherKey, otherKeyValue) :: otherPairs ->
           if key = otherKey
           then otherKeyValue
           else alGetting otherPairs
  in alGetting pairs ;;

(* AL PUT. Return a new association list that is like PAIRS, but associates KEY
   with VALUE. We assume KEY is not already a key in PAIRS, though AL PUT will
   still work if it is. *)

let alPut pairs key value =
  (key, value) :: pairs ;;

(* PROPOSITION. An expression in propositional logic. *)

type proposition =
  False |                                (* false *)
  True |                                 (* true *)
  Var   of string |                      (* a, b, c, etc. *)
  Not   of proposition |                 (* ¬ R *)
  And   of proposition * proposition |   (* L ∧ R *)
  Or    of proposition * proposition |   (* L ∨ R *)
  Imply of proposition * proposition |   (* L → R *)
  Equiv of proposition * proposition ;;  (* L ↔ R *)

(* EVALUATE. Evaluate a PROPOSITION to an OCaml Boolean, given the bindings of
   its names in PAIRS. Recall that α → β ≡ ¬ α ∨ β, and that α ↔ β ≡ α = β. We
   dispatch to code that evaluates each connective. *)

let evaluate proposition pairs =
  let rec evaluating proposition =
    match proposition
    with False               -> false |
         True                -> true |
         Var name            -> alGet pairs name |
         Not right           -> not (evaluating right) |
         And (left, right)   -> evaluating left && evaluating right |
         Or (left, right)    -> evaluating left || evaluating right |
         Imply (left, right) -> not (evaluating left) || (evaluating right) |
         Equiv (left, right) -> evaluating left = evaluating right
  in evaluating proposition ;;

(* PRINT BOOLS. Print a list of Booleans BOOLS as a series of T's and F's. End
   with a newline. *)

let rec printBools bools =
  match bools
  with [] ->
         printf "\n" |
       first :: rest ->
         printf "%s" (if first then "T" else "F") ;
         printBools rest ;;

(* For example, the following expression prints FFT. *)

printBools [false ; false ; true] ;;

(* PRINT PAIRS. Print an association list PAIRS that associates string keys
   with Booleans. End with a newline. Keys are printed without quotes and their
   values are printed as T's and F's. *)

let rec printPairs pairs =
  match pairs
  with [] ->
         printf "\n" |
       (key, value) :: otherPairs ->
         printf "%s → %s " key (if value then "T" else "F") ;
         printPairs otherPairs ;;

(* For example, the following expression prints c → T b → F a → T. *)

let l = alPut [] "a" true
in let l = alPut l "b" false
   in let l = alPut l "c" true
      in printPairs l ;;

(* We'll now develop some generators that produce all possible sequences of
   FALSE's and TRUE's. Whenever one of them generates a sequence, it calls its
   continuation ETC on that sequence. *)

(* GENERATE BOOLS. Generate all possible lists of Boolean values, of length N.
   We assume N ≥ 0. This isn't used in the tautology checker. *)

let generateBools etc n =
  let rec generating bools n =
    match n
    with 0 ->
           etc bools |
         _ ->
           generating (false :: bools) (n - 1) ;
           generating (true  :: bools) (n - 1)
  in generating [] n ;;

(* For example, the following expression prints:

FFF
TFF
FTF
TTF
FFT
TFT
FTT
TTT

*)

generateBools printBools 3 ;;

(* GENERATE PAIRS. We'll change GENERATE BOOLS slightly so it makes association
   lists. Generate all possible association lists that associate the strings in
   the list NAMES with Boolean values. This isn't used in the tautology checker
   either. *)

let generatePairs etc names =
  let rec generating names pairs =
    match names
    with [] ->
           etc pairs |
         name :: otherNames ->
           generating otherNames (alPut pairs name false) ;
           generating otherNames (alPut pairs name true)
  in generating names [] ;;

(* For example, the following expression prints:

c → F b → F a → F 
c → T b → F a → F 
c → F b → T a → F 
c → T b → T a → F 
c → F b → F a → T 
c → T b → F a → T 
c → F b → T a → T 
c → T b → T a → T 

*)

generatePairs printPairs ["a" ; "b" ; "c"] ;;

(* GENERATE AND TEST PAIRS. Here we change GENERATE PAIRS slightly again. Now
   it not only generates association lists, but it tests if each generated list
   makes its continuation ETC return true. All we did is change the function's
   name, and change its ';' to '&&'. This is used in the tautology checker. *)

let generateAndTestPairs etc names =
  let rec generating names pairs =
    match names
    with [] ->
           etc pairs |
         name :: otherNames ->
           generating otherNames (alPut pairs name false) &&
           generating otherNames (alPut pairs name true)
  in generating names [] ;;

(* We'll need a function NAMES that returns a list of names in a PROPOSITION.
   Its external helpers IS IN and UNIQUIFY are defined first. *)

(* IS IN. Test if THING is a member of the list THINGS. It's tail recursive. *)

let isIn thing things =
  let rec isInning things =
    match things
    with [] ->
           false |
         firstThing :: otherThings ->
           (thing = firstThing) || (isInning otherThings)
  in isInning things ;;

(* UNIQUIFY. Return a list like THINGS, but in which no element appears more
   than once. We don't care about the order of elements, so we'll make it tail
   recursive and use '::'. *)

let uniquify things =
  let rec uniquifying things uniqueThings =
    match things
    with [] ->
           uniqueThings |
         firstThing :: otherThings ->
           if isIn firstThing uniqueThings
           then uniquifying otherThings uniqueThings
           else uniquifying otherThings (firstThing :: uniqueThings)
  in uniquifying things [] ;;

(* NAMES. Return a list of strings that represent the names in PROPOSITION. No
   string appears in the list more than once. The operator '@' appends two
   lists. We dispatch to code that gets the names from each connective. *)

let names proposition =
  let rec namesing proposition =
    match proposition
    with False               -> [] |
         True                -> [] |
         Var name            -> [name] |
         Not right           -> namesing right |
         And (left, right)   -> namesing left @ namesing right |
         Or (left, right)    -> namesing left @ namesing right |
         Imply (left, right) -> namesing left @ namesing right |
         Equiv (left, right) -> namesing left @ namesing right
  in uniquify (namesing proposition) ;;

(* The big finish! *)

(* IS TAUTOLOGY. Test if PROPOSITION is a tautology. If PROPOSITION contains n
   distinct names, then this runs in O(2ⁿ) exponential time ☹. That means we
   should call it only on PROPOSITION's with small numbers of names. *)

let isTautology proposition =
  generateAndTestPairs
    (fun pairs -> evaluate proposition pairs)
    (names proposition) ;;

(* For example, the proposition ¬ (p ∧ q) → (¬ p ∨ ¬ q) is a tautology, so the 
   following expression returns TRUE. *)

isTautology
  (Imply
    (Not
      (And
        (Var "p", Var "q")),
     Or
      (Not
        (Var "p"),
       Not
        (Var "q")))) ;;

(* But p ∧ q is not a tautology, so the following expression returns FALSE. *)

isTautology
  (And
    (Var "p", Var "q")) ;;
