(* 
   History
    Lisp
    LCF
    OCaml *)

(* Lisp: List Processor *)
(* lambda calculus model of function application *)


(* Modern programming languages consider a source file as a series of tokens not characters *)
(* let add x y = x + y;; *) 
(* token: significant series of chars, ignores blanks, new lines, comments*)
(* scanner, tokenizer, lexer, lexical scanner: *)
            (* Turns sequence of chars to sequence of tokens*)
(* parser *) 
            (* turns a sequence of tokens into some internal representation*)


(* hello 31 (big dogs) *)
module Scanner = 
struct type token = 
    CloseParenToken|
    OpenParenToken|
    EndToken|
    NumberToekn of int|
    SymbolToken of string;;

let makeScanner path = 
    let input = open_in path in 
        let ch = ref ' ' (* character constant*)in 
            (* keep reading next token *)



            let rec nextToken() = 
                let let nextChar () = (* increase ch variable to the next one*)
                try ch:= input_char input
                with End_of_File -> ch:='\000'
                in
                    let nextBloseParen() = 
                                    nextChar();CloseParenToken
                        in let rec nextComment () = 
                            match !ch with 
                            |'\000' -> ()
                            |'\n' -> nextChar()
                            |_ -> nextChar();nextComment()
                            let nextEndToken () = EndToken 
                                    in let nextOpenParenToken () = 
                                        nextChar();CloseParenToken
                                        in let nextSymbolToken prefix = 
                                            let rec nextSymboling chars = 
                                                match !ch with 
                                                | '\000'|'\n'|'('|')'->
                                                    SymbolToken chars 
                                                | _ -> let otherChars = Char.escaped !ch 
                                                        in nextChar ();
                                                            nextSymboling (chars ^ otherChars)
                                                in nextSymboling prefix
                                            in let nextNumberToken prefix = 
                                                let rec nextNumbering chars = 
                                                    match !ch with 
                                                    |'\000'|'\n'|' '|'('|')'-> 
                                                        NumberToekn (int_of_string chars)
                                                    |_ -> 
                                                        let otherChars = Char.escaped !ch in 
                                                        nextChar ();
                                                        nextNumbering (chars ^ otherChars)
                                                    in nextNumbering prefix
                                                in let nextNumberOrSymbolToken prefix = 
                                                    nextChar ();
                                                    match !ch with 
                                                    |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> nextNumberToken "-"
                                                    |_ -> nextSymbolToken "-" in 
                match !ch with 
                    '\000' -> 
                        nextEndToken ()(* character at the end of the file *)|
                    ' '|'\n' -> nextChar ();nextToken ()|
                    '(' -> nextOpenParenToken ()|
                    ')' -> nextCloseParenToken ()|
                    ';' -> nextComment();nextToken ()|
                    '-' -> nextNumberOrSymbolToken ()|
                    '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> nextNumberToken()|
                    _ -> (*symbol*) nextSymbolToken prefix
                in nextChar();nextToken;;





(* Grammer - formal description of a set of strings. strings may be of tokens, chars.
   CFG Context-free grammer; Roles, productions
   Syntax diagram       Nicholos Wirth*)


(* e.g.  *)
(append ((quote))quo(te()));;

let envMake() = [];;

let global = ref envMake();;

let envPut name value env = 
    (name,value)::env;;


global := envPut "nil" Nil (!global);;
let tee = Symbol "t";;
global := envPut "t" tee (!global);;


type thing = 
        |Closure of thing * thing * environment
        |Cons of thing * thing 
        |Nil
        |Number of integer 
        |Symbol of string 
    and 
            environment = (string*thing) list;;

module type Evaluators = 
    sig 
        val evaluate: thing -> thing;;
        exception EvaluatorError of string;;
    end;;

module Evaluator:Evaluators = 
    struct 
        exception EvaluatorError of string;;
        let oops message = raise (EvaluatorError message);;
        let envGet env name etc = 
            let envGetting env = 
                match env with 
                |[] -> etc () 
                |(otherName, otherValue)::otherEnv -> 
                    if name=otherName then otherValue
                    else envGetting otherEnv
            in envGetting env;;


let lookup env name = 
    envGet env name (fun () -> 
        envGet (!global) name (fun () -> 
            oops "Unbounded name " ^ name));;



let rec evaluating thing env = 
    match thing with 
    |Cons (func,args) -> 
        (match (evaluating func env) with 
        |Closure (pars,body,bodyEnv) -> apply pars args body bodyEnv 
        |Primitive howTo -> howTo args env
        |_ -> oops "Closure or primitive expected")
    |Symbol name -> 
        lookup env name|
    _ -> thing;;

let evalute thing = 
    evaluating thing (envMake ());;

let primitive name howTo = 
    global := envPut name (Primitive howTo) (!global);;

let primitive quote (fun args _ -> 
                            match args with 
                            |Cons(arg,_)->arg 
                            |_ -> oops "quote excepted one argument");;



