%code top {
	#include <stdio.h>
	#include "lexer.hpp"
}

%code requires {
	#include <string>
	#include "ast.hpp"

}

%code {
	SymbolTableFun STFun;
	SymbolTableVar STVar;
	IRSymbolTableFun IRSTFun;
    IRSymbolTableVar IRSTVar;
}

%locations


%token T_byte		  "byte"
%token T_else		  "else"
%token T_false		  "false"
%token T_if 		  "if"
%token T_int 		  "int"
%token T_proc		  "proc"
%token T_reference 	  "reference"
%token T_return       "return"
%token T_while 		  "while"
%token T_true 		  "true"

%token<num> T_int_lit
%token<char_lit> T_char_lit
%token<str> T_string_lit
%token<id> T_id

%token<op> T_eq 	  "=="
%token<op> T_neq 	  "!="
%token<op> T_geq 	  ">="
%token<op> T_leq 	  "<="
%token<op> T_greater  ">"
%token<op> T_less 	  "<"

%left<op> '|'
%left<op> '&'
//%nonassoc T_eq
//%nonassoc T_neq
//%nonassoc T_geq
//%nonassoc T_leq
//%nonassoc T_greater
//%nonassoc T_less
%left '+' '-'
%left '*' '/' '%'
%precedence '!'

%expect 1

%union {
	Type *type;
	RType * r_type;
	FParList *fpar_list;
	FParDef *fpar_def;
	FuncDef *func_def;
	LocalDefLr *local_def_list;
	LocalDef *local_def;
	VarDef *var_def;
	Stmt *stmt;
	CompoundStmt *comp_stmt;
	StmtList *stmt_lsit;
	FuncCall *func_call;
	ExprList *expr_list;
	Expr *expr;
	Lvalue *l_value;
	Cond *cond;

	Datatype data_type;
	int num;
    char op;
    char char_lit;
	std::string *id;
	std::string *str;
}

%type<func_def> func_def
%type<fpar_list> fpar_list
%type<fpar_def> fpar_def
%type<data_type> data_type
%type<type> type
%type<r_type> r_type
%type<local_def_list> local_def_lr
%type<local_def> local_def
%type<var_def> var_def
%type<stmt> stmt
%type<stmt_lsit> stmt_list
%type<comp_stmt> compound_stmt
%type<func_call> func_call
%type<expr_list> expr_list
%type<expr> expr
%type<l_value> l_value
%type<cond> cond

%%

program : func_def 									{ //std::cout << *$1 << std::endl; 
													  $1->sem();
													  $1->compile_and_dump();}
		; 

func_def : T_id  '('  ')' ':' r_type  local_def_lr compound_stmt 			{ $$ = new FuncDef(nullptr, $5,$6,$7,$1); $$->set_line(yylineno); }
		 | T_id  '('  fpar_list   ')' ':' r_type local_def_lr compound_stmt 	{ $$ = new FuncDef($3, $6,$7,$8,$1); $$->set_line(yylineno);}
		 					
		 ;

fpar_list : fpar_list ',' fpar_def						   { $1->append($3); $$ = $1; } 
		  | fpar_def							   { $$=new FParList(); $$->append($1);$$->set_line(yylineno);  }
		  ;

fpar_def : T_id ':' "reference" type						   { $$= new FParDef($1,*$4,true); $$->set_line(yylineno);}
		 | T_id ':'  type                   				   { $$= new FParDef($1,*$3,false);$$->set_line(yylineno);}
		 ;

data_type : "int" 							           { $$ = DATATYPE_int; }
		  | "byte"							   { $$ = DATATYPE_byte; }
		  ;

type : data_type  '[' ']' 							   { $$ = new Type($1, true); $$->set_line(yylineno);}
	 | data_type 								   { $$ = new Type($1, false); $$->set_line(yylineno);}
	 ;

r_type : data_type 								   { $$ = new RType(false, $1); $$->set_line(yylineno);}
	  | "proc"								   { $$ = new RType(true); $$->set_line(yylineno);}
	  ;

local_def_lr : local_def_lr local_def       					   { $1->append($2); $$ = $1; }
			 | %empty                       			   { $$ = new LocalDefLr();$$->set_line(yylineno);}
			 ;

local_def : func_def                        				           { $$ = new LocalDef(nullptr,$1); $$->set_line(yylineno);}
		  | var_def                         			           { $$ = new LocalDef($1,nullptr);  $$->set_line(yylineno);}
		  ;

var_def : T_id ':' data_type '[' T_int_lit ']' ';' 				   { $$ = new VarDef($1,true,$3,$5);$$->set_line(yylineno);}
	    | T_id ':' data_type  ';'                  				   { $$ = new VarDef($1,false,$3);$$->set_line(yylineno);}
	    ;

stmt : ';'								           { $$ = new EmptyStmt();$$->set_line(yylineno);}
	 | l_value '=' expr ';'						           { $$ = new Assignment($1,$3); $$->set_line(yylineno);}
	 | compound_stmt							   { $$ = $1; }
	 | func_call ';'							   { $$ = new StmtFCall($1); $$->set_line(yylineno);}
	 | "if" '(' cond ')' stmt 					   	   { $$ = new StmtIfCond($3,$5,nullptr); $$->set_line(yylineno);}
	 | "if" '(' cond ')' stmt "else" stmt 		   			   { $$ = new StmtIfCond($3,$5,$7); $$->set_line(yylineno);}
	 | "while" '(' cond ')' stmt 				   	           { $$ = new WhileStmt($3,$5); $$->set_line(yylineno);}
	 | "return"  expr ';'						           { $$ = new StmtRet($2);$$->set_line(yylineno);}
	 | "return" ';'								   { $$ = new StmtRet(nullptr);$$->set_line(yylineno);}
	 ;

stmt_list : stmt_list stmt						   	   {$1->append($2);$$=$1;}
		  | %empty						           {$$=new StmtList();$$->set_line(yylineno);}
		  ;

compound_stmt : '{' stmt_list '}' 				   		   { $$ = new CompoundStmt($2); $$->set_line(yylineno);}	   
			  ;

func_call : T_id '(' expr_list ')'                  			           { $$ = new FuncCall($1,$3);$$->set_line(yylineno);}
		  | T_id '(' ')'						   { $$ = new FuncCall($1,nullptr);$$->set_line(yylineno);}
		  ;

expr_list : expr_list ',' expr					   		   { $1->append($3);$$=$1;}
		 | expr							           { $$=new ExprList(); $$->append($1);  $$->set_line(yylineno);}
		 ;

expr : T_int_lit  								   { $$ = new ExprLitInt($1); $$->set_line(yylineno);}
	 | T_char_lit 								   { $$ = new ExprLitChar($1); $$->set_line(yylineno);}
	 | l_value  								   {  $$ = $1; }
	 | '(' expr ')' 							   { $$ = $2; }
	 | func_call 								   { $$ = $1; }
	 | '+' expr 								   { $$ = new ExprUnitaryOp($2,'+'); $$->set_line(yylineno);}
	 | '-' expr 								   { $$ = new ExprUnitaryOp($2,'-'); $$->set_line(yylineno);}
	 | expr '+' expr							   { $$ = new ExprBinaryOp($1,$3,'+'); $$->set_line(yylineno);}
	 | expr '-' expr							   { $$ = new ExprBinaryOp($1,$3,'-'); $$->set_line(yylineno);}
	 | expr '*' expr							   { $$ = new ExprBinaryOp($1,$3,'*'); $$->set_line(yylineno);}
	 | expr '/' expr							   { $$ = new ExprBinaryOp($1,$3,'/'); $$->set_line(yylineno);}
	 | expr '%' expr							   { $$ = new ExprBinaryOp($1,$3,'%'); $$->set_line(yylineno);}
	 ;


l_value : T_id '[' expr ']' 					   { $$ = new LvalueId($1,$3); $$->set_line(yylineno);}
	    | T_id   								   { $$ = new LvalueId($1,nullptr); $$->set_line(yylineno);}
	    | T_string_lit 							   { $$ = new LvalueStr($1);$$->set_line(yylineno);}
	    ;

cond : "true" 									   { $$ = new CondConst(true); $$->set_line(yylineno);}
	 | "false" 									   { $$ = new CondConst(false); $$->set_line(yylineno);}
	 | '(' cond ')' 							   { $$ = $2; }
	 | '!' cond									   { $$ = new NotCond($2); $$->set_line(yylineno);}
 	 | expr  T_eq expr							   { $$ = new CompareCond($1,$3,$2);$$->set_line(yylineno);}
 	 | expr T_neq expr							   { $$ = new CompareCond($1,$3,$2);$$->set_line(yylineno);}
 	 | expr T_less expr							   { $$ = new CompareCond($1,$3,$2);$$->set_line(yylineno);}
 	 | expr T_greater expr						   { $$ = new CompareCond($1,$3,$2);$$->set_line(yylineno);}
 	 | expr T_geq expr							   { $$ = new CompareCond($1,$3,$2);$$->set_line(yylineno);}
 	 | expr T_leq expr							   { $$ = new CompareCond($1,$3,$2);$$->set_line(yylineno);}
 	 | cond '&' cond							   { $$ = new CondOp($1,$3,$2);$$->set_line(yylineno);}
 	 | cond '|' cond							   { $$ = new CondOp($1,$3,$2);$$->set_line(yylineno);}
 	 ;


%%
void yyerror(const char *msg,int linenumber) {
  fprintf(stderr, "Semantic error in line %d: %s\n", linenumber,msg);
  exit(42);  
}
void yyerror(const char *msg) {
  fprintf(stderr, "Syntax error in line %d: %s\n", yylineno,msg);
  exit(42);
}



int main () {
	return yyparse();
}


