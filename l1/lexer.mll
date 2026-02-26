(* File lexer.mll *)
{
  open Parser
  open Lexing 

(* next_line copied from  Ch. 16 of "Real Workd Ocaml" *) 
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let newline = ('\010' | "\013\010" )
let loc_reg_exp = 'l'['0'-'9']+
let int_reg_exp = ['0'-'9']+

	rule token = parse
	  | [' ' '\t']     { token lexbuf }     (* skip blanks *)
	  | '+'            { ADD }
	  | '-'            { SUB }
	  | ">="		   { GEQ }
	  | ';'	           { SEMICOLON }
	  | ":="		   { ASSIGN }
	  | "!" 	 	   { BANG }
	  | "true" 		   { TRUE }
	  | "false"    	   { FALSE }
	  | "if" 		   { IF }
	  | "then" 		   { THEN } 
	  | "else" 	  	   { ELSE }
	  | "skip" 	 	   { SKIP }
	  | "while" 	   { WHILE }
	  | "do"		   { DO }
	  | "done"  	   { DONE }
	  | int_reg_exp { INT (int_of_string (Lexing.lexeme lexbuf)) }
	  | loc_reg_exp { LOC (Lexing.lexeme lexbuf) }
	  | "(*" { comment lexbuf; token lexbuf }
	  | newline { next_line lexbuf; token lexbuf } 
	  | eof { EOF }
	  | _ { Errors.complain ("Lexer : Illegal character " ^ (Char.escaped(Lexing.lexeme_char lexbuf 0))) }

and comment = parse
  | "*)" { () }
  | newline { next_line lexbuf; comment lexbuf }
  | "(*" {comment lexbuf; comment lexbuf }
  | eof {Errors.complain "ERROR: Lexer. Comment not terminated. "}
  | _ { comment lexbuf } 