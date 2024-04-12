(*
  MODULES. An example OCaml module.

    James Moen
    20 Mar 24

  This module has code for association lists (AL's).
*)

(* AL TYPE. The type of the module AL. There's a type for each of its members.
   If we leave out a type, then the corresponding member isn't visible outside
   the module. *)

module type AlType =
sig
 type ('key, 'value) t = Empty | Node of 'key * 'value * ('key, 'value) t ;;
 exception Error of string ;;
 val isEmpty : ('key, 'value) t -> bool ;;
 val isIn : 'key -> ('key, 'value) t -> bool ;;
 val make : unit -> ('key, 'value) t ;;
 val put : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t ;;
 val get : 'key -> ('key, 'value) t -> 'value ;;
end ;;

(* AL. A module full of functions that work on AL's. We could omit the colon
   and the type AL TYPE on the next line. If we do that then OCaml deduces AL's
   type automatically (but does not define AL TYPE). *)

module Al : AlType =
struct

(* T. By convention, if a module defines a type and operations on instances of
   that type, then the type is called (lower case) T. Here the type describes
   a linear linked list of KEY's and VALUE's. *)

 type ('key, 'value) t =
  Empty |
  Node of 'key * 'value * ('key, 'value) t ;;

(* ERROR. An exception, raised if we try to GET the value of a KEY that's not
   in an AL. *)

 exception Error of string;;

(* IS EMPTY. Test if AL has no keys and no values. *)

 let isEmpty al =
  al = Empty ;;

(* IS IN. Test if KEY is a key in AL. *)

 let isIn key al =
  let rec isInning al =
   match al
   with Empty ->
         false |
        Node (otherKey, _, otherAl) ->
         key = otherKey || isInning otherAl
  in isInning al ;;

(* MAKE. Return a new empty AL. *)

 let make () =
  Empty ;;

(* GET. Return the value of KEY in AL. If KEY isn't in AL then raise ERROR. *)

 let get key al =
  let rec getting al =
   match al
   with Empty ->
         raise (Error "No such key") |
        Node (otherKey, otherValue, otherAl) ->
         if key = otherKey
         then otherValue
         else getting otherAl
  in getting al ;;

(* PUT. Return a new AL that's like AL except that KEY is now associated with
   VALUE. *)

 let put key value al =
  Node (key, value, al) ;;
end ;;
