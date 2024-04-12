(*
  STACK. Simulate an object oriented stack using a function closure.

    James Moen
    23 Feb 24
*)

open List ;; (* We'll need HD, TL, and (::). *)

(* STACK OPERATION. A stack operation, duh. IS EMPTY tests if a stack is empty,
   PEEK returns the top element of the stack, POP deletes the top element, and
   PUSH adds a new top element. *)

type
  'base stackOperation =
    StackOpIsEmpty |
    StackOpPeek |
    StackOpPop |
    StackOpPush of 'base ;;

(* STACK RESULT. The result of a stack operation. BASE RESULT is an object from
   the stack, BOOL RESULT is a Boolean, and NO RESULT is no object. *)

type
  'base stackResult =
    StackBaseResult of 'base |
    StackBoolResult of bool |
    StackNoResult ;;

(* STACK ERROR. Raised when we can't PEEK or POP. *)

exception StackError of string ;;

(* MAKE STACK. Return a closure DISPATCHER that represents a stack. TOP is the
   list of objects on the stack. It's top is the first element of that list.
   DISPATCHER takes a STACK OPERATION and returns a STACK RESULT. *)

let makeStack () =
  let top = ref []
  in let dispatcher operation =
       match operation
       with StackOpIsEmpty ->
              StackBoolResult (! top = []) |
            StackOpPeek ->
              if ! top = []
              then raise (StackError "Can't peek.")
              else StackBaseResult (hd (! top)) |
            StackOpPop ->
              if ! top = []
              then raise (StackError "Can't pop.")
              else (top := tl (! top) ;
                    StackNoResult) |
            StackOpPush base ->
              top := base :: (! top) ;
              StackNoResult
     in dispatcher ;;

(* Some examples. Make a new empty stack S. *)

let s = makeStack () ;;

(* Its type is '_weak1 stackOperation -> '_weak1 stackResult. OCaml doesn't
   know S's base type, so it uses the weak type name '_weak1 to represent it.
   '_weak1 is replaced by a known type as soon as OCaml knows S's base type.
   We'll push a string "C" on the stack. *)

s (StackOpPush "C") ;;

(* Now OCaml should know S's base type is STRING. Let's see. *)

s ;;

(* S's base type is string stackOperation -> string stackResult. The weak type
   was replaced by a known type. We'll do more pushes and pops. *)

s (StackOpPush "B") ;;
s (StackOpPush "A") ;;

(* Now "A", "B" and "C" are on the stack. If we peek then we'll get "A". *)

s StackOpPeek ;;

(* If we pop and peek again we'll get "B". *)

s StackOpPop ;;
s StackOpPeek ;;

(* And if we pop and peek one more time we'll get "C" *)

s StackOpPop ;;
s StackOpPeek ;;

(* The last pop empties the stack, so we'll get TRUE. *)

s StackOpPop ;;
s StackOpIsEmpty ;;
