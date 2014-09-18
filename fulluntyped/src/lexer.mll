{
open Lexing
open Parser

(* Error handling *)
exception Error of string

let err_with_info lexbuf str =
  let pos = lexbuf.lex_curr_p in
  let err_str =
    Printf.sprintf "%d:%d: %s"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol) str
  in
  let err_str =
    if String.length pos.pos_fname != 0 then
      String.concat ":" [pos.pos_fname; err_str]
    else
      err_str
  in
  err_str

let lex_err lexbuf =
  let lexeme = Lexing.lexeme lexbuf in
  let str = Printf.sprintf "Illegal Character: %s" (Char.escaped lexeme.[0]) in
  raise (Error (err_with_info lexbuf str))

(* Utils *)
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }

let text = Lexing.lexeme
}

let white = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | "let"   { LET }
  | "in"    { IN }
  | '\\'    { LAMBDA }
  | '.'     { DOT }
  | '='     { EQ }
  | '('     { L_PAR }
  | ')'     { R_PAR }
  | ';'     { SEMICOL }
  | id      { ID (text lexbuf) }
  | _       { lex_err lexbuf }
  | eof     { EOF }
