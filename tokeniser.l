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

stringconst  \"[^\n"]+\"
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
"("		return RPARENT;
")"		return LPARENT;
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
"begin"	return BEGIN0;
"true"  return TRUE0;
"false" return FALSE0;
"DIPLAY" return DIPLAY;
"var"    return VAR;
"BOOLEAN" return BOOLEAN;
"INTEGER" return INTEGER;
"DOUBLE"  return DOUBLE;
"CHAR" return CHAR;
{id}		return ID;

<<EOF>>		return FEOF;
{ws}    {/* skip blanks and tabs */};
"(*"    { /* Skip comments between '(*' and '*)' */
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

{unknown}	return UNKNOWN;

%%

