//  A compiler from a very simple Pascal-like structured language LL(k)
//  to 64-bit 80x86 Assembly langage
//  Copyright (C) 2019 Pierre Jourlin
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Build with "make compilateur"


#include <string>
#include <iostream>
#include <cstdlib>
#include <set>
#include <FlexLexer.h>
#include "tokeniser.h"
#include <cstring>
#include <map>


using namespace std;

enum OPREL {EQU, DIFF, INF, SUP, INFE, SUPE, WTFR};
enum OPADD {ADD, SUB, OR, WTFA};
enum OPMUL {MUL, DIV, MOD, AND ,WTFM};

TOKEN current;				// Current token


FlexLexer* lexer = new yyFlexLexer; // This is the flex tokeniser
// tokens can be read using lexer->yylex()
// lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h)
// and lexer->YYText() returns the lexicon entry as a string

	
enum TYPE { TYPE_UNDEFINED, TYPE_UNSIGNED_INT, TYPE_BOOLEAN };
map<string, TYPE> VariablesWithTypes;
unsigned long TagNumber=0;

bool IsDeclared(const char *id){
	return VariablesWithTypes.count(id);
}


void Error(string s){
	cerr << "Ligne n°"<<lexer->lineno()<<", lu : '"<<lexer->YYText()<<"'("<<current<<"), mais ";
	cerr<< s << endl;
	exit(-1);
}

void TypeError(string message){
	cerr << "Ligne n°" << lexer->lineno() << ", erreur de type sur '" 
	<< lexer->YYText() << "' (" << current << ") : " << message << endl;
	exit(-1);
}

string TypeToString(TYPE t) {
	switch(t) {
		case TYPE_UNSIGNED_INT: return "UNSIGNED_INT";
		case TYPE_BOOLEAN: return "BOOLEAN";
		case TYPE_UNDEFINED: return "UNDEFINED";
		default: return "UNKNOWN";
	}
}

// Program := [DeclarationPart] StatementPart
// DeclarationPart := "[" Letter {"," Letter} "]"
// StatementPart := Statement {";" Statement} "."
// Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
// WhileStatement := "WHILE" Expression "DO" Statement
// ForStatement := "FOR" AssignementStatement "To" Expression "DO" Statement
// BlockStatement := "BEGIN" Statement { ";" Statement } "END"
// AssignementStatement := Letter "=" Expression

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
// SimpleExpression := Term {AdditiveOperator Term}
// Term := Factor {MultiplicativeOperator Factor}
// Factor := Number | Letter | "(" Expression ")"| "!" Factor
// Number := Digit{Digit}

// AdditiveOperator := "+" | "-" | "||"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// Letter := "a"|...|"z"
	
		
TYPE Identifier(void){
	cout << "\tpush "<<lexer->YYText()<<endl;
	string name = lexer->YYText();
	current=(TOKEN) lexer->yylex();
	return VariablesWithTypes[name];
}

TYPE Number(void) {
	string txt = lexer->YYText();

	// Check if the string really is an unsigned int
	for (char c : txt) {
		if (!isdigit(c)) {
			TypeError("Erreur de syntaxe : '" + txt + "' n'est pas une constante entière positive valide.");
		}
	}

	cout << "\tpush $" << atoi(txt.c_str()) << endl;

	current = (TOKEN) lexer->yylex();

	return TYPE_UNSIGNED_INT;
}

TYPE Expression(void);			// Called by Term() and calls Term()

TYPE Factor(void) {
	if (current == RPARENT) {
		current = (TOKEN) lexer->yylex();  // skip '('

		TYPE exprType = Expression();      // parse inner expression

		if (current != LPARENT)
			Error("')' était attendu");
		else
			current = (TOKEN) lexer->yylex();  // skip ')'

		return exprType;
	}
	else if (current == NUMBER) {
		return Number();
	}
	else if (current == ID) {
		return Identifier();
	}
	else {
		Error("'(' ou chiffre ou lettre attendue");
		return TYPE_UNDEFINED;  // Optional fallback
	}
}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator(void){
	OPMUL opmul;
	if(strcmp(lexer->YYText(),"*")==0)
		opmul=MUL;
	else if(strcmp(lexer->YYText(),"/")==0)
		opmul=DIV;
	else if(strcmp(lexer->YYText(),"%")==0)
		opmul=MOD;
	else if(strcmp(lexer->YYText(),"&&")==0)
		opmul=AND;
	else opmul=WTFM;
	current=(TOKEN) lexer->yylex();
	return opmul;
}

// Term := Factor {MultiplicativeOperator Factor}
TYPE Term(void){
	OPMUL mulop;
	TYPE type1 = Factor();
	while(current==MULOP){
		mulop=MultiplicativeOperator();		// Save operator in local variable
		TYPE type2 = Factor();
		if (type1 != type2) {
			string op;
			switch(mulop) {
				case MUL: op = "*"; break;
				case DIV: op = "/"; break;
				case MOD: op = "%"; break;
				case AND: op = "&&"; break;
				default:  op = "??"; break;
			}
			TypeError("Types incompatibles dans l'expression : impossible d'appliquer l'opérateur '" + op +
					  "' entre " + TypeToString(type1) + " et " + TypeToString(type2));
		}

		cout << "\tpop %rbx"<<endl;	// get first operand
		cout << "\tpop %rax"<<endl;	// get second operand
		switch(mulop){
			case AND:
				cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
				cout << "\tpush %rax\t# AND"<<endl;	// store result
				break;
			case MUL:
				cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
				cout << "\tpush %rax\t# MUL"<<endl;	// store result
				break;
			case DIV:
				cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
				cout << "\tdiv %rbx"<<endl;			// quotient goes to %rax
				cout << "\tpush %rax\t# DIV"<<endl;		// store result
				break;
			case MOD:
				cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
				cout << "\tdiv %rbx"<<endl;			// remainder goes to %rdx
				cout << "\tpush %rdx\t# MOD"<<endl;		// store result
				break;
			default:
				Error("opérateur multiplicatif attendu");
		}
	}
	return type1;
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator(void){
	OPADD opadd;
	if(strcmp(lexer->YYText(),"+")==0)
		opadd=ADD;
	else if(strcmp(lexer->YYText(),"-")==0)
		opadd=SUB;
	else if(strcmp(lexer->YYText(),"||")==0)
		opadd=OR;
	else opadd=WTFA;
	current=(TOKEN) lexer->yylex();
	return opadd;
}

// SimpleExpression := Term {AdditiveOperator Term}
TYPE SimpleExpression(void){
	OPADD adop;
	TYPE type1 = Term();
	while(current==ADDOP){
		adop=AdditiveOperator();		// Save operator in local variable
		TYPE type2 = Term();			// Get next term

		if (type1 != type2) {
			string op;
			switch(adop) {
				case ADD: op = "+"; break;
				case SUB: op = "-"; break;
				case OR:  op = "||"; break;
				default:  op = "??"; break;
			}
			TypeError("Types incompatibles dans l'expression : impossible d'appliquer l'opérateur '" + op +
					  "' entre " + TypeToString(type1) + " et " + TypeToString(type2));
		}

		cout << "\tpop %rbx"<<endl;	// get first operand
		cout << "\tpop %rax"<<endl;	// get second operand
		switch(adop){
			case OR:
				cout << "\taddq	%rbx, %rax\t# OR"<<endl;// operand1 OR operand2
				break;			
			case ADD:
				cout << "\taddq	%rbx, %rax\t# ADD"<<endl;	// add both operands
				break;			
			case SUB:	
				cout << "\tsubq	%rbx, %rax\t# SUB"<<endl;	// substract both operands
				break;
			default:
				Error("opérateur additif inconnu");
		}
		cout << "\tpush %rax"<<endl;			// store result
	}
	return type1;
}

// DeclarationPart := "[" Ident {"," Ident} "]"
void DeclarationPart(void){
	if(current!=RBRACKET)
		Error("caractère '[' attendu");
	cout << "\t.data"<<endl;
	cout << "\t.align 8"<<endl;
	
	current=(TOKEN) lexer->yylex();
	if(current!=ID)
		Error("Un identificater était attendu");
	cout << lexer->YYText() << ":\t.quad 0"<<endl;
	string varName = lexer->YYText();
	if (VariablesWithTypes.count(varName)) {
		Error("Variable '" + varName + "' déjà déclarée");
	}
	VariablesWithTypes[varName] = TYPE_UNSIGNED_INT;
	current=(TOKEN) lexer->yylex();
	while(current==COMMA){
		current=(TOKEN) lexer->yylex();
		if(current!=ID)
			Error("Un identificater était attendu");
		cout << lexer->YYText() << ":\t.quad 0"<<endl;
		string varName = lexer->YYText();
		if (VariablesWithTypes.count(varName)) {
			Error("Variable '" + varName + "' déjà déclarée");
		}
		VariablesWithTypes[varName] = TYPE_UNSIGNED_INT;
		current=(TOKEN) lexer->yylex();
	}
	if(current!=LBRACKET)
		Error("caractère ']' attendu");
	current=(TOKEN) lexer->yylex();
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
OPREL RelationalOperator(void){
	OPREL oprel;
	if(strcmp(lexer->YYText(),"==")==0)
		oprel=EQU;
	else if(strcmp(lexer->YYText(),"!=")==0)
		oprel=DIFF;
	else if(strcmp(lexer->YYText(),"<")==0)
		oprel=INF;
	else if(strcmp(lexer->YYText(),">")==0)
		oprel=SUP;
	else if(strcmp(lexer->YYText(),"<=")==0)
		oprel=INFE;
	else if(strcmp(lexer->YYText(),">=")==0)
		oprel=SUPE;
	else oprel=WTFR;
	current=(TOKEN) lexer->yylex();
	return oprel;
}

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
TYPE Expression(void){
	OPREL oprel;
	if (current == TRUE0)
	{
		cout << "\tpush $0xFFFFFFFFFFFFFFFF\t\t# True"<<endl;	
		current = (TOKEN) lexer->yylex();  // Advance to next token
		return TYPE_BOOLEAN;
	} else if (current == FALSE0)
	{
		cout << "\tpush $0\t\t# False"<<endl;
		current = (TOKEN) lexer->yylex();  // Advance to next token
		return TYPE_BOOLEAN;
	}
	
	int tag = ++TagNumber;
	TYPE type1 = SimpleExpression();
	if(current==RELOP){
		oprel=RelationalOperator();
		TYPE type2 = SimpleExpression();
		if (type1 != type2)
			TypeError("Comparaison invalide : l'expression de gauche est de type " + 
					TypeToString(type1) + ", mais celle de droite est de type " + 
					TypeToString(type2));

		cout << "\tpop %rax"<<endl;
		cout << "\tpop %rbx"<<endl;
		cout << "\tcmpq %rax, %rbx"<<endl;
		switch(oprel){
			case EQU:
				cout << "\tje Vrai"<<tag<<"\t# If equal"<<endl;
				break;
			case DIFF:
				cout << "\tjne Vrai"<<tag<<"\t# If different"<<endl;
				break;
			case SUPE:
				cout << "\tjae Vrai"<<tag<<"\t# If above or equal"<<endl;
				break;
			case INFE:
				cout << "\tjbe Vrai"<<tag<<"\t# If below or equal"<<endl;
				break;
			case INF:
				cout << "\tjb Vrai"<<tag<<"\t# If below"<<endl;
				break;
			case SUP:
				cout << "\tja Vrai"<<tag<<"\t# If above"<<endl;
				break;
			default:
				Error("Opérateur de comparaison inconnu");
		}
		cout << "\tpush $0\t\t# False"<<endl;
		cout << "\tjmp Suite"<<tag<<endl;
		cout << "Vrai"<<tag<<":\tpush $0xFFFFFFFFFFFFFFFF\t\t# True"<<endl;	
		cout << "Suite"<<tag<<":"<<endl;
		return TYPE_BOOLEAN;
	} else
	{
		return type1;
	}
}

void Statement(void);

// AssignementStatement := Identifier ":=" Expression
string AssignementStatement(void){
	string variable;
	if(current!=ID)
		Error("Identificateur attendu");
	if(!IsDeclared(lexer->YYText())){
		cerr << "Erreur : Variable '"<<lexer->YYText()<<"' non déclarée"<<endl;
		exit(-1);
	}

	variable=lexer->YYText();
	TYPE type1 = VariablesWithTypes[variable];

	current=(TOKEN) lexer->yylex();
	if(current!=ASSIGN)
		Error("caractères ':=' attendus");
	current=(TOKEN) lexer->yylex();
	TYPE type2 = Expression();
	if (type1 != type2) {
		TypeError("Affectation invalide : la variable '" + variable + 
				  "' est de type " + TypeToString(type1) + 
				  ", mais l'expression est de type " + TypeToString(type2));
	}
	cout << "\tpop "<<variable<<endl;
	return variable;
}

// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
void IfStatement(void){
	int ifTag = ++TagNumber; // reserve a unique tag once

	current = (TOKEN) lexer->yylex(); // read after 'IF'
	TYPE TypeCondition = Expression();
	if (TypeCondition != TYPE_BOOLEAN)
		TypeError("Condition in IF statement must be of type BOOLEAN, but got '" 
				+ string(lexer->YYText()) + "' of type " 
				+ TypeToString(TypeCondition));	

	cout << "\tpop %rax" << endl;
	cout << "\tcmp $0, %rax" << endl;

	if (current != THEN)
		Error("Expected 'then'");

	cout << "\tje Else" << ifTag << "\t# If condition is false, jump to Else" << endl;

	current = (TOKEN) lexer->yylex();
	Statement();

	cout << "\tjmp Suite" << ifTag << endl;

	cout << "Else" << ifTag << ":" << endl;
	if (current == ELSE) {
		current = (TOKEN) lexer->yylex();
		Statement();
	}
	cout << "Suite" << ifTag << ":" << endl;
}

void WhileStatement(void) {
	int whileTag = ++TagNumber;

	// Start of loop label
	cout << "While" << whileTag << ":" << endl;

	current = (TOKEN) lexer->yylex(); // read after 'WHILE'
	TYPE TypeCondition = Expression();
	if (TypeCondition != TYPE_BOOLEAN)
		TypeError("Condition invalide dans la boucle WHILE : expression de type " 
			+ TypeToString(TypeCondition) + " au lieu de BOOLEAN.");


	cout << "\tpop %rax" << endl;
	cout << "\tcmp $0, %rax" << endl;

	if (current != DO)
		Error("Expected 'DO' after WHILE condition");

	// Jump if condition is false
	cout << "\tje EndWhile" << whileTag << "\t# Exit loop if false" << endl;

	current = (TOKEN) lexer->yylex();
	Statement();  // body of the loop

	// Jump back to start
	cout << "\tjmp While" << whileTag << endl;

	// End label
	cout << "EndWhile" << whileTag << ":" << endl;
}

void ForStatement(void){
	int step = 1;
	int forTag = ++TagNumber;

	current = (TOKEN) lexer->yylex(); // read after 'FOR'
	string CurrentAssignedVar = AssignementStatement();

	cout << "\tmov " << CurrentAssignedVar << ", %rax" << endl;

	if (current != TO) Error("Expected 'TO'");
	current = (TOKEN) lexer->yylex();

	Expression();  // limit value
	cout << "\tpop %rdx" << endl;

	if (current != DO) Error("Expected 'DO'");
	current = (TOKEN) lexer->yylex();

	// Optional STEP
	if (current == STEP) {
		current = (TOKEN) lexer->yylex();
		if (current != NUMBER) Error("Expected number after STEP");
		step = atoi(lexer->YYText());
		current = (TOKEN) lexer->yylex();
	}

	// Jump to test before first iteration
	cout << "\tjmp TestFor" << forTag << endl;

	// Loop body
	cout << "LoopFor" << forTag << ":" << endl;
	Statement();  // loop body

	// Increment
	cout << "\tadd $" << step << ", %rax" << endl;
	cout << "\tmov %rax, " << CurrentAssignedVar << endl;

	// Test condition
	cout << "TestFor" << forTag << ":" << endl;
	cout << "\tcmp %rax, %rdx" << endl;
	cout << "\tjb LoopFor" << forTag << "\t# If still less than limit, continue" << endl;

	// End of loop
	cout << "EndFor" << forTag << ":" << endl;
}

// BlockStatement := "BEGIN" Statement { ";" Statement } "END"
void BeginStatement(void){
	current = (TOKEN) lexer->yylex(); 
	Statement();

	while(current == SEMICOLON){
		current = (TOKEN) lexer->yylex();
		Statement();
	}

	if (current != END)
		Error("Expected 'END'");

	current = (TOKEN) lexer->yylex();
}

// Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
void Statement(void){
	if (current == ID)
	{
		AssignementStatement();
	} else if (current == IF)
	{
		IfStatement();
	} else if (current == WHILE)
	{
		WhileStatement();
	} else if (current == FOR)
	{
		ForStatement();
	}
	else if (current == BEGIN0)
	{
		BeginStatement();
	}
	else {
		Error("A weird input!");
	}
}

// StatementPart := Statement {";" Statement} "."
void StatementPart(void){
	cout << "\t.text\t\t# The following lines contain the program"<<endl;
	cout << "\t.globl main\t# The main function must be visible from outside"<<endl;
	cout << "main:\t\t\t# The main function body :"<<endl;
	cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top"<<endl;
	Statement();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();
		Statement();
	}
	if(current!=DOT)
		Error("caractère '.' attendu");
	current=(TOKEN) lexer->yylex();
}

// Program := [DeclarationPart] StatementPart
void Program(void){
	if(current==RBRACKET)
		DeclarationPart();
	StatementPart();	
}

int main(void){	// First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	cout << "\t\t\t# This code was produced by the CERI Compiler"<<endl;
	// Let's proceed to the analysis and code production
	current=(TOKEN) lexer->yylex();
	Program();
	// Trailer for the gcc assembler / linker
	cout << "\tmovq %rbp, %rsp\t\t# Restore the position of the stack's top"<<endl;
	cout << "\tret\t\t\t# Return from main function"<<endl;
	if(current!=FEOF){
		cerr <<"Caractères en trop à la fin du programme : ["<<current<<"]";
		Error("."); // unexpected characters at the end of program
	}

}