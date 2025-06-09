%{
// This is our Lexical tokeniser 
// It should be compiled into cpp with :
// flex++ -d -otokeniser.cpp tokeniser.l 
// And then compiled into object with
// g++ -c tokeniser.cpp
// tokens can be read using lexer->yylex()
// lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h)
// and lexer->YYText() returns the lexicon entry as a string

#include "tokeniser.h"
#include <iostream>

using namespace std;

%}

%option noyywrap
%option c++
%option yylineno
%option caseless    

stringconst  \"[^\n\"]*\"
ws      [ \t\n\r]+
alpha   [A-Za-z]
digit   [0-9]
charConst	\'[^\']\' 
float 	[0-9]+\.[0-9]+
number  {digit}+
id	{alpha}({alpha}|{digit})*
addop	(\+|\-|\|\|)
mulop	(\*|\/|%|\&\&)
relop	(\<|\>|"=="|\<=|\>=|!=)
unknown [^\"A-Za-z0-9 \n\r\t\(\)\<\>\=\!\%\&\|\}\-\;\.]+

%%

{addop}		return ADDOP;
{mulop}		return MULOP;
{relop}		return RELOP;
{charConst}	return CHARCONST;
{number}	return NUMBER;
{float}		return FLOATCONST;
{stringconst}	return STRINGCONST;
"["		return RBRACKET;
"]"		return LBRACKET;
","		return COMMA;
";"		return SEMICOLON;
"."		return DOT;
":="	return ASSIGN;
":"		return COLON;
"("		return LPARENT;
")"		return RPARENT;
"!"		return NOT;
"then"	return THEN;
"else"	return ELSE;
"do"	return DO;
"to"	return TO;
"step"	return STEP;
"end"	return END;
"if"	return IF;
"while"	return WHILE;
"repeat"	return REPEAT;
"until"	return UNTIL;
"with" return WITH;
"for"	return FOR;
"case"	return CASE;
"of"	return OF;
"function" return FUNCTION;
"procedure"	return PROCEDURE;
"begin"	return BEGIN0;
"true"  return TRUE0;
"false" return FALSE0;
"DISPLAY" return DISPLAY;
"var"    return VAR;
"BOOLEAN" return BOOLEAN;
"INTEGER" return INTEGER;
"DOUBLE"  return DOUBLE;
"CHAR" return CHAR;
"STRING" return STRING;
{id}		return ID;
\{[^}]*\}               ;        // Pascal-style comments
\/\/[^\n]*              ;        // C++-style comments
[ \t\r\n]+              ;        // Skip whitespace

"(*"    { // Pascal-style block comments
    int c;
    while((c = yyinput()) != 0){
        if(c == '*'){
            if((c = yyinput()) == ')')
                break;
            else
                unput(c);
        }	
    }
};


<<EOF>>		return FEOF;

{unknown}	return UNKNOWN;

%%

