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

enum OPREL {EQU, DIFF, INF, SUP, INFE, SUPE};
enum OPADD {ADD, SUB, OR};
enum OPMUL {MUL, DIV, MOD, AND};

TOKEN current;				// Current token


FlexLexer* lexer = new yyFlexLexer; // This is the flex tokeniser
// tokens can be read using lexer->yylex()
// lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h)
// and lexer->YYText() returns the lexicon entry as a string

	
enum TYPE { TYPE_UNDEFINED, TYPE_UNSIGNED_INT, TYPE_BOOLEAN, TYPE_CHAR, TYPE_DOUBLE };
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
		case TYPE_CHAR: return "CHAR";
		case TYPE_DOUBLE: return "DOUBLE";
		case TYPE_UNDEFINED: return "UNDEFINED";
		default: return "UNKNOWN";
	}
}

TYPE CheckBinaryOperationTypes(TYPE t1, TYPE t2, string op) {
	if (t1 == TYPE_UNDEFINED || t2 == TYPE_UNDEFINED || t1 == TYPE_CHAR || t2 == TYPE_CHAR)
		TypeError("Opération non supportée pour CHAR ou UNDEFINED");

	if ((t1 == TYPE_BOOLEAN || t2 == TYPE_BOOLEAN) && t1 != t2)
		TypeError("Les booléens ne peuvent être combinés qu'entre eux");

	if ((op == "&&" || op == "||") && (t1 != TYPE_BOOLEAN || t2 != TYPE_BOOLEAN))
		TypeError("'" + op + "' ne peut s'appliquer qu'à des booléens");

	if ((op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=") &&
	    (t1 == TYPE_BOOLEAN && t2 == TYPE_BOOLEAN))
		return TYPE_BOOLEAN; // only ==, != should reach here

	if ((op == "%" && (t1 != TYPE_UNSIGNED_INT || t2 != TYPE_UNSIGNED_INT)))
		TypeError("L'opérateur '%' ne s'applique qu'à deux entiers");

	if (t1 == t2)
		return t1;

	if ((t1 == TYPE_UNSIGNED_INT && t2 == TYPE_DOUBLE) || (t1 == TYPE_DOUBLE && t2 == TYPE_UNSIGNED_INT))
		return TYPE_DOUBLE;

	TypeError("Types incompatibles pour '" + op + "' entre " + TypeToString(t1) + " et " + TypeToString(t2));
	return TYPE_UNDEFINED; // not reached
}


// Program := [DeclarationPart] StatementPart
// VarDeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
// VarDeclaration := Ident {"," Ident} ":" Type


// StatementPart := Statement {";" Statement} "."
// Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
// WhileStatement := "WHILE" Expression "DO" Statement
// ForStatement := "FOR" AssignementStatement "To" Expression "DO" Statement
// BlockStatement := "BEGIN" Statement { ";" Statement } "END"
// AssignementStatement := Letter "=" Expression
// DisplayStatement := "DISPLAY" Expression

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
	string name = lexer->YYText();
	cout << "\tpush " << name << endl;
	current = (TOKEN) lexer->yylex();
	return VariablesWithTypes[name];
}

TYPE Number(void) {
	string txt = lexer->YYText();
	cout << "\tpush $" << txt << "\t# push constant" << endl;
	current = (TOKEN) lexer->yylex();
	return VariablesWithTypes[txt];
}

TYPE CharConst(void) {
	char c = lexer->YYText()[1];  // lexer->YYText() is like: `'a'`
	cout << "\tmovq $0, %rax" << endl;
	cout << "\tmovb $" << (int)c << ", %al" << endl;
	cout << "\tpush %rax\t# push char '" << c << "'" << endl;
	current = (TOKEN) lexer->yylex();
	return TYPE_CHAR;
}

TYPE Expression(void);			// Called by Term() and calls Term()

TYPE Factor(void) {
	if (current == LPARENT) {
		current = (TOKEN) lexer->yylex();  // skip '('

		TYPE exprType = Expression();      // parse inner expression

		if (current != RPARENT)
			Error("')' était attendu");
		else
			current = (TOKEN) lexer->yylex();  // skip ')'

		return exprType;
	}
	else if (current == NUMBER) {
		return Number();
	}
	else if (current == CHARCONST) {
		return CharConst();
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
	std::string op = lexer->YYText();
	if(op == "*")
		opmul=MUL;
	else if(op == "/")
		opmul=DIV;
	else if(op == "%")
		opmul=MOD;
	else if(op == "&&")
		opmul=AND;
	else Error("Opérateur multiplicatif invalide. Attendus: * | / | % | && ");;
	current=(TOKEN) lexer->yylex();
	return opmul;
}

// Term := Factor {MultiplicativeOperator Factor}
TYPE Term(void){
	OPMUL mulop;
	TYPE type1 = Factor();

	while(current == MULOP){
		mulop = MultiplicativeOperator();
		TYPE type2 = Factor();

		string op;
		switch(mulop) {
			case MUL: op = "*"; break;
			case DIV: op = "/"; break;
			case MOD: op = "%"; break;
			case AND: op = "&&"; break;
			default:  op = "??"; break;
		}

		TYPE commonType = CheckBinaryOperationTypes(type1, type2, op);

		if (commonType == TYPE_DOUBLE)
		{
			cout << "\tfldl (%rsp)           # Load top of general stack (b) into %st(0)" << endl;
			cout << "\taddq $8, %rsp         # Pop b" << endl;
			cout << "\tfldl (%rsp)           # Load next (a) into %st(0), b is now in %st(1)" << endl;
			cout << "\taddq $8, %rsp         # Pop a" << endl;

			switch(mulop){
				case MUL:
					cout << "\tfmulp %st, %st(1)       # DOUBLE MUL" << endl;
					break;
				case DIV:
					cout << "\tfdivp %st, %st(1)       # DOUBLE DIV" << endl;
					break;
				default:
					Error("opérateur multiplicatif attendu");
			}

			cout << "\tsubq $8, %rsp         # Make space on general stack" << endl;
			cout << "\tfstpl (%rsp)          # Store result back to general stack" << endl;

			type1 = commonType;
		} else {
			cout << "\tpop %rbx" << endl;
			cout << "\tpop %rax" << endl;

			switch(mulop){
				case AND:
					cout << "\tmulq %rbx" << endl;
					cout << "\tpush %rax\t# AND" << endl;
					break;
				case MUL:
					cout << "\tmulq %rbx" << endl;
					cout << "\tpush %rax\t# MUL" << endl;
					break;
				case DIV:
					cout << "\tmovq $0, %rdx" << endl;
					cout << "\tdiv %rbx" << endl;
					cout << "\tpush %rax\t# DIV" << endl;
					break;
				case MOD:
					cout << "\tmovq $0, %rdx" << endl;
					cout << "\tdiv %rbx" << endl;
					cout << "\tpush %rdx\t# MOD" << endl;
					break;
				default:
					Error("opérateur multiplicatif attendu");
			}

			type1 = commonType;
		}
	}

	return type1;
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator(void){
	OPADD opadd;
	std::string op = lexer->YYText();
	if(op == "+")
		opadd=ADD;
	else if(op == "-")
		opadd=SUB;
	else if(op == "||")
		opadd=OR;
	else Error("Opérateur additif invalide. Attendus: + , -, ||");
	current=(TOKEN) lexer->yylex();
	return opadd;
}

// SimpleExpression := Term {AdditiveOperator Term}
// SimpleExpression := Term {AdditiveOperator Term}
TYPE SimpleExpression(void){
	OPADD adop;
	TYPE type1 = Term();

	while(current==ADDOP){
		adop = AdditiveOperator();		// Save operator in local variable
		TYPE type2 = Term();			// Get next term

		string op;
		switch(adop) {
			case ADD: op = "+"; break;
			case SUB: op = "-"; break;
			case OR:  op = "||"; break;
			default:  op = "??"; break;
		}

		TYPE commonType = CheckBinaryOperationTypes(type1, type2, op);

		if (commonType == TYPE_DOUBLE)
		{
			cout << "\tfldl (%rsp)           # Load top of general stack (b) into %st(0)" << endl;
			cout << "\taddq $8, %rsp         # Pop b" << endl;
			cout << "\tfldl (%rsp)           # Load next (a) into %st(0), b is now in %st(1)" << endl;
			cout << "\taddq $8, %rsp         # Pop a" << endl;

			switch(adop){
				case ADD:
					cout << "\tfaddp %st, %st(1)       # DOUBLE ADD" << endl;
					break;
				case SUB:
					cout << "\tfsubp %st, %st(1)       # DOUBLE SUB" << endl;
					break;
				default:
					Error("opérateur additif attendu");
			}

			cout << "\tsubq $8, %rsp         # Make space on general stack" << endl;
			cout << "\tfstpl (%rsp)          # Store result back to general stack" << endl;

			type1 = commonType;
		} else {
			cout << "\tpop %rbx" << endl;
			cout << "\tpop %rax" << endl;

			switch(adop){
				case OR:
					cout << "\torq %rbx, %rax\t# OR" << endl;
					break;
				case ADD:
					cout << "\taddq %rbx, %rax\t# ADD" << endl;
					break;
				case SUB:
					cout << "\tsubq %rbx, %rax\t# SUB" << endl;
					break;
				default:
					Error("opérateur additif inconnu");
			}

			cout << "\tpush %rax" << endl;
			type1 = commonType;
		}
	}

	return type1;
}

TYPE Type(void){
	TYPE type;
	if (current == BOOLEAN)	
	{
		type = TYPE_BOOLEAN;
	} else if (current == INTEGER)
	{
		type = TYPE_UNSIGNED_INT;
	} else if (current == DOUBLE)
	{
		type = TYPE_DOUBLE;
	} else if (current == CHAR)
	{
		type = TYPE_CHAR;
	} else {
		type = TYPE_UNDEFINED;
	}
		
	current=(TOKEN) lexer->yylex();
	return type;
}

// VarDeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
void VarDeclarationPart(void){
	if(current!=VAR) 	Error("caractère 'VAR' attendu");
	cout << "\t.data"<<endl;
	cout << "\tFormatString1:    .string \"%llu\\n\"" << endl;
	cout << "\tFormatString2:    .string \"%f\\n\"" << endl;
	cout << "\tFormatString3:    .string \"%c\\n\"" << endl;
	cout << "\t.align 8"<<endl;
	VarDeclaration();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();
		VarDeclaration();
	}
	if(current!=DOT)
		Error("caractère '.' attendu");
	current=(TOKEN) lexer->yylex();
}

// VarDeclaration := Ident {"," Ident} ":" Type
void VarDeclaration(void){

	if(current!=ID)
		Error("Un identificater était attendu");

	string varName;
	std::set<std::string> variablesSet;

	varName = lexer->YYText();
	variablesSet.insert(varName);

	current=(TOKEN) lexer->yylex();
	while(current==COMMA){
		current=(TOKEN) lexer->yylex();
		if(current!=ID)
			Error("Un identificater était attendu");
		//cout << lexer->YYText() << ":\t.quad 0"<<endl;
		varName = lexer->YYText();
		if (variablesSet.find(varName) != variablesSet.end()) {
			// It's already in the set
			std::cerr << "Variable '" << varName << "' is already declared in this block.\n";
		} else {
			variablesSet.insert(varName);
		}
		current=(TOKEN) lexer->yylex();
	}

	if(current!=COLON)
		Error("caractère ':' attendu");
	current=(TOKEN) lexer->yylex();

	TYPE type = Type();

	if (type == TYPE_UNDEFINED) {
		Error("Type non reconnu ou mal écrit");
	}

	for (const std::string& var : variablesSet) {
		if (VariablesWithTypes.count(var)) {
			Error("Variable '" + var + "' déjà déclarée");
		}
		VariablesWithTypes[var] = type;
		//cout << lexer->YYText() << ":\t.word 0     # 32-bit unsigned integer"<<endl;
		if (type == TYPE_UNSIGNED_INT)	
		{
			cout << var << ":\t.quad 0     # 32-bit unsigned integer"<<endl;
		} else if (type == TYPE_BOOLEAN)
		{
			cout << var << ":\t.byte 0        # 1 byte boolean"<<endl;
		} else if (type == TYPE_CHAR)
		{
			cout << var << ":\t.byte 0      # character"<<endl;
		} else if (type == TYPE_DOUBLE)
		{
			cout << var << ":\t.double 0.0  # 64-bit double"<<endl;
		}
	}
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
OPREL RelationalOperator(void){
	OPREL oprel;
	std::string op = lexer->YYText();
	if(op == "==")
		oprel=EQU;
	else if(op == "!=")
		oprel=DIFF;
	else if(op == "<")
		oprel=INF;
	else if(op == ">")
		oprel=SUP;
	else if(op == "<=")
		oprel=INFE;
	else if(op == ">=")
		oprel=SUPE;
	else Error("Opérateur de comparaison invalide. Attendus: == != < > <= >=");
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
		oprel = RelationalOperator();
		TYPE type2 = SimpleExpression();

		// Determine the operator string for CheckBinaryOperationTypes
		string opString;
		switch(oprel){
			case EQU:  opString = "=="; break;
			case DIFF: opString = "!="; break;
			case INF:  opString = "<"; break;
			case SUP:  opString = ">"; break;
			case INFE: opString = "<="; break;
			case SUPE: opString = ">="; break;
			default:   opString = "??"; break;
		}

		// Use CheckBinaryOperationTypes to check type compatibility
		TYPE commonType = CheckBinaryOperationTypes(type1, type2, opString);

		if (commonType == TYPE_DOUBLE) {
			cout << "\tfldl (%rsp)       # b in st(0)" << endl;
			cout << "\taddq $8, %rsp" << endl;
			cout << "\tfldl (%rsp)       # a in st(0), b in st(1)" << endl;
			cout << "\taddq $8, %rsp" << endl;
			cout << "\tfucomip %st(0), %st(1)" << endl;
			cout << "\tfstp %st(0)" << endl;
		} else {
			cout << "\tpop %rax" << endl;
			cout << "\tpop %rbx" << endl;
			cout << "\tcmpq %rax, %rbx" << endl;
		}

		switch(oprel){
			case EQU:
				cout << "\tje Vrai" << tag << "\t# If equal" << endl;
				break;
			case DIFF:
				cout << "\tjne Vrai" << tag << "\t# If different" << endl;
				break;
			case SUPE:
				cout << "\tjae Vrai" << tag << "\t# If above or equal" << endl;
				break;
			case INFE:
				cout << "\tjbe Vrai" << tag << "\t# If below or equal" << endl;
				break;
			case INF:
				cout << "\tjb Vrai" << tag << "\t# If below" << endl;
				break;
			case SUP:
				cout << "\tja Vrai" << tag << "\t# If above" << endl;
				break;
			default:
				Error("Opérateur de comparaison inconnu");
		}

		cout << "\tpush $0\t\t# False" << endl;
		cout << "\tjmp Suite" << tag << endl;
		cout << "Vrai" << tag << ":\tpush $0xFFFFFFFFFFFFFFFF\t\t# True" << endl;
		cout << "Suite" << tag << ":" << endl;
		return TYPE_BOOLEAN;
	} else {
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
	TYPE loopVarType = VariablesWithTypes[CurrentAssignedVar];
	if (loopVarType != TYPE_UNSIGNED_INT) {
		TypeError("FOR loop variable must be INTEGER");
	}
	cout << "\tmov " << CurrentAssignedVar << ", %rax" << endl;

	if (current != TO) Error("Expected 'TO'");
	current = (TOKEN) lexer->yylex();

	TYPE toType = Expression();  // limit value
	if (toType != TYPE_UNSIGNED_INT) {
		TypeError("FOR loop to value must be INTEGER");
	}

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

void DisplayStatement(void){
	current = (TOKEN) lexer->yylex(); 
	TYPE ExprType = Expression();

	if (ExprType == TYPE_UNSIGNED_INT || ExprType == TYPE_BOOLEAN) {
		cout << "\tpop %rdx                     # The value to be displayed" << endl;
		cout << "\tmovq $FormatString1, %rsi    # '%llu\\n'" << endl;
	} else if (ExprType == TYPE_DOUBLE) {
		cout << "\tmovsd (%rsp), %xmm0          # Load double into xmm0" << endl;
		cout << "\taddq $8, %rsp                # Pop stack" << endl;
		cout << "\tmovq $FormatString2, %rsi    # '%f\\n'" << endl;
	} else if (ExprType == TYPE_CHAR) {
		cout << "\tpop %rdx                     # The value to be displayed" << endl;
		cout << "\tmovq $FormatString3, %rsi    # '%c\\n'" << endl;
	} else {
		Error("DISPLAY ne supporte que les types INTEGER, DOUBLE et CHAR.");
	}

	cout << "\tmovl $1, %edi" << endl;
	cout << "\tmovl $0, %eax" << endl;
	cout << "\tcall __printf_chk@PLT" << endl;

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
	} else if (current == DISPLAY)
	{
		DisplayStatement();
	} else {
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
	if(current==VAR)
		VarDeclarationPart();
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