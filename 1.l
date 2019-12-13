%{
#include <stdio.h>
#include "y.tab.h"
/*int yylineno = 0;*/
%}
%option noyywrap
id [a-zA-Z][a-zA-Z0-9]*
%%
"/*"(.|\n\r)+"*/" ;
"!int" {yylval.intval=1; return TIP_INT;}
"!float" {yylval.intval=5;return TIP_FLOAT;}
"!char" {yylval.intval=4;return TIP_CHAR;}
"!string" {yylval.intval=2;return TIP_STRING;}
"!bool" {yylval.intval=3;return TIP_BOOL;}
"!class" {return TIP_CLASS;}
"!if" {return IF_CLAUSE;}
"!while" {return WHILE_CLAUSE;}
"!for" {return FOR_CLAUSE;}
"!>" {return BIGGER;}
"!<" {return SMALLER;}
"!==" {return EQUAL;}
"!&&" {return AND;}
"!||" {return OR;}
"+++" {return PLUS;}
"---" {return MINUS;}
"***" {return MUL;}
"///" {return DIV;}
"!definefunction" {return FUN;}
"<()>" {return APEL;}
"!const" {return CONSTANT;}
"!strcpy" {return STRCPY;}
"!strcat" {return STRCAT;}
"!strlength" {return STRLENGTH;}
"YES" {yylval.intval=1;return BOOL;}
"NO" {yylval.intval=0;return BOOL;}
"^"[A-Z][a-z]* {yylval.strval=strdup(yytext);return ID_VAR;}
[A-Za-z] {yylval.charval=(strdup(yytext)[0]);return LITERA;}
[A-Za-z]+ {yylval.strval=strdup(yytext);return LITEREE;}
[0-9]+ {yylval.intval=atoi(yytext);return NR;}
[ \t] ;
\r\n {yylineno++;}
. {return yytext[0];}
