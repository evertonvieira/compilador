%{
	#include "TVII.tab.h"
	#include <ctype.h>
	#include <stdlib.h>
	#include <stdio.h>
	#include <string.h>
	char *yytext;
	YYSTYPE yylval;
%}

ALFA 			[A-Za-z_]
PLUS            [\+]
MINUS           [\-]
TIMES           [\*]
DIVIDE          [/]
DIGIT           [0-9]
NUMBER          ({DIGIT}+)
PRINT           "print"

IF              "if"
ELSE            "else"
TINT            "int"
WHILE           "while"
DO              "do"
BEGIN           "begin"
END             "end"

IDENT			{ALFA}({ALFA}|{DIGIT})*
WS              [ \t]*
ATRIB			"="
LP              "("
RP              ")"
PTV             ";"
VIRGULA         ","
RET             [\n]

%%

{WS}            {
                /* eat up white space */
                }
{PLUS}          {
                return _MAIS;
                }
{TIMES}         {
                return _MULT;
                }
{MINUS}         {
                return _MENOS;
                }
{DIVIDE}        {
                return _DIVID;
                }
{ATRIB}			{
				return _ATRIB;
				}
{BEGIN} 		{
				return _BEGIN;
				}
{END} 			{
				return _END;
				}
{LP}            {
                return _ABREPAR;
                }
{RP}            {
                return _FECHAPAR;
                }
{VIRGULA}       {
                return _VIRGULA;
                }
{PTV}			{
				return _PTVIRG;
				}
{TINT}			{
				return _TINT;
				}
{NUMBER}        {
                strcpy(yylval.t.symbol, yytext);
                return _N;
                }
{PRINT}         {
                return _PRINT;
                }
{IF}			{
				return _IF;
				}
{ELSE}          {
                return _ELSE;
                }
{WHILE}         {
                return _WHILE;
                }
{DO}            {
                return _DO;
                }
{IDENT}         {
				strcpy(yylval.t.symbol, yytext);
				return _V;
				}
{RET}           {
                return yytext[0];
                }
