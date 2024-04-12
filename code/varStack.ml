(*
  VAR STACK. A stack made from a mutable linear linked list.

    James Moen
    29 Feb 24

  Among other things this demonstrates how a higher order function (here called
  ENSURE NONEMPTY) can let us factor out repeated code from other functions.

*)

(* STACK ERROR. Raised when we can't perform a stack operation. The STRING is
   an error message. *)

exception StackError of string ;;

(* STACK NODE. A node in a singly linked linear list. EMPTY STACK NODE is an
   empty list, and STACK NODE is part of a nonempty one. Each STACK NODE has
   a BASE slot and a pointer to the next node in the list (if it exists). *)

type 'base stackNode =
  EmptyStackNode |
  StackNode of 'base * 'base stackNode ref ;;

(* STACK. A linked stack. It has a pointer to the top STACK NODE. *)

type 'base stack =
  Stack of 'base stackNode ref ;;

(* IS EMPTY. Test if STACK is empty. *)

let isEmpty stack =
  match stack
  with Stack top ->
         ! top = EmptyStackNode ;;

(* MAKE STACK. Return a new empty STACK. *)

let makeStack () =
  Stack (ref EmptyStackNode) ;;

(* ENSURE NONEMPTY. If STACK is empty then raise STACK ERROR with an error
   MESSAGE string in it. Otherwise call the continuation ETC on the TOP of the
   stack, the THING in the first STACK NODE, and the pointer to the NEXT STACK
   NODE. *)

let ensureNonempty etc stack message =
  match stack
  with Stack top ->
        (match ! top
         with EmptyStackNode ->
                raise (StackError message) |
              StackNode (thing, next) ->
                etc top thing next) ;;

(* PEEK. Return the THING at the top of the stack, if possible. *)

let peek stack =
  ensureNonempty (fun _ thing _ -> thing) stack "Can't peek" ;;

(* POP. Delete the thing at the TOP of the stack, if possible. *)

let pop stack =
  ensureNonempty (fun top _ next -> top := ! next) stack "Can't pop" ;;

(* PUSH. Push a new THING on top of a STACK. *)

let push stack thing =
  match stack
  with Stack top ->
         top := StackNode (thing, ref ! top) ;;

(*
  If ENSURE NONEMPTY was not used, then PEEK and POP would need repetitious
  code, like this:

  let peek stack =
    match stack
    with Stack top ->
          (match ! top
           with EmptyStackNode ->
                  raise (StackError "Can't peek.") |
                StackNode (thing, _) ->
                  thing) ;;

  let pop stack =
    match stack
    with Stack top ->
          (match ! top
           with EmptyStackNode ->
                  raise (StackError "Can't pop.") |
                StackNode (_, next) ->
                  top := ! next) ;;
*)
