%{
#include "lexer.hpp"
#include "parser.tab.h"
#define T_eof   0

int linenum = 1;
int block_comment_nestings = 0;

%}
L [A-Za-z]
D [0-9]
HD [0-9A-Fa-f]
W [ \t\r]
C {L}|{D}|[ !#%&()*+,-./:;<=>?\[\]^_{|}~]|\\n|\\t|\\r|\\0|\\\\|\\'|\\\"|\\x{HD}{2}

%option noyywrap
%option warn
%option yylineno


%x block_comment
%%

"byte"    	{ return T_byte; }
"else"    	{ return T_else; }
"false"   	{ return T_false; }
"if"  	  	{ return T_if; }
"int"		{ return T_int; }
"proc"    	{ return T_proc; }
"reference" { return T_reference; }
"return"	{ return T_return; }	
"while" 	{ return T_while; }
"true"  	{ return T_true; }

"=="		{ yylval.op='e' ;return T_eq; }
"!="		{ yylval.op='i' ;return T_neq; }
"<="		{ yylval.op='l' ;return T_leq; }
">="		{ yylval.op='g' ;return T_geq; }
">"			{ yylval.op='>' ;return T_greater; }
"<"			{ yylval.op='<' ;return T_less; }

[=+\-*/%!&|()\[\]{},:;]  { yylval.op = yytext[0];return yytext[0]; }

{L}({L}|{D}|_)*     { yylval.id = new std::string(yytext,yyleng);return T_id; }
{D}+    	    { yylval.num=std::atoi(yytext);return T_int_lit; }
'{C}'	    { if(yytext[1]=='\\'){
            switch(yytext[2]){
              case 'n': yylval.char_lit='\n';break;
              case 't': yylval.char_lit='\t';break;
              case 'r': yylval.char_lit='\r';break;
              case '0': yylval.char_lit='\0';break;
              case '\\': yylval.char_lit='\\';break;
              case '\'': yylval.char_lit='\'';break;
              case '\"': yylval.char_lit='\"';break;
              case 'x': 
                  char hex[3];  
                  hex[0] = yytext[2];
                  hex[1] = yytext[3];
                  hex[2] = '\0';                
                  yylval.char_lit=(char)strtol(hex, NULL, 16);break;
            }}
            else{
                yylval.char_lit=yytext[1];
            }
          return T_char_lit; }
\"{C}*\"	    {  yylval.str = new std::string(yytext+1,yyleng-2);return T_string_lit; }
{W}+    	{ /* nothing */ }
"(*"		{ ++block_comment_nestings; BEGIN(block_comment); }

<INITIAL,block_comment>\n      { ++linenum; }

<block_comment>"(*"	{ ++block_comment_nestings; }
<block_comment>"*)"	{ if(--block_comment_nestings == 0) BEGIN(INITIAL); }
<block_comment>[^\n(*]	{ /* nothing */ }


--.*\n		{ ++linenum; }
--.*"<<EOF>>"	{ /* nothing */ }


<INITIAL,block_comment>.       { fprintf(stderr, "Illegal character %c at line %d\n", yytext[0], linenum);}


%%

#ifdef TOKEN_DBG
int main() {
  int token;
  printf("lexer testing\n");
  do {
    token = yylex();
    printf("token = %d, lexeme = %s\n", token, yytext);
  } while (token != T_eof);
  printf("linenum: %d\n", linenum);
  return 0;
}
#endif