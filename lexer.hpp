#ifndef __LEXER_HPP__
#define __LEXER_HPP__
extern int yylineno;

int yylex(void);
void yyerror(const char *msg);
#endif
