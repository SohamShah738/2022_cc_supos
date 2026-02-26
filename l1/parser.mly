/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <string> LOC
%token <int> INT
%token ADD SUB GEQ SEMICOLON
%token IF THEN ELSE SKIP WHILE DO DONE
%token TRUE FALSE
%token EOF ASSIGN BANG

%left ADD SUB        /* lowest precedence */
%left GEQ      /* medium precedence */
%left ASSIGN

%nonassoc UMINUS        /* highest precedence */


%start main
%type <Past.expr> simple_expr 
%type <Past.expr> expr 
%type <Past.expr> expr1
%type <Past.expr list> exprlist
%type <Past.expr> main

%%
main:
	expr1 EOF                { $1 }
;

simple_expr:
| SKIP 								 { Past.Skip (get_loc())           }
| TRUE								 { Past.Boolean (get_loc(), true)  }
| FALSE								 { Past.Boolean (get_loc(), false) }
| LOC								 { Past.Loc (get_loc(), $1)        }
| INT                                { Past.Integer (get_loc(), $1)    }
| BANG LOC					 		 { Past.Deref(get_loc(), $2)       }

expr:
| simple_expr                        {  $1 }
| expr ADD expr                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr GEQ expr                      { Past.Op(get_loc(), $1, Past.GEQ, $3) }
| LOC ASSIGN expr                   { Past.Assign(get_loc(), $1, $3) 		}

expr1: 
| expr								 { $1						      }
| exprlist 							 { Past.Seq(get_loc(), $1)		  }
| IF expr THEN expr ELSE expr		 { Past.If(get_loc(), $2, $4, $6) }
| WHILE expr DO expr DONE			 { Past.While(get_loc(), $2, $4)  }

exprlist:
|   expr                             { [$1]      }
|   expr  SEMICOLON exprlist         { $1 :: $3  }


