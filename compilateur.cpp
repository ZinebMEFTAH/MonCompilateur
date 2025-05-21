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

#include <vector>
#include <string>
#include <iostream>
#include <cstdlib>
#include <set>
#include <FlexLexer.h>
#include "tokeniser.h"
#include <cstring>
#include <map>
#include <map>
#include <cstdint>  


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


// StatementPart := StructuredStatement {";" StructuredStatement} "."
// StructuredStatement := <compound statement> | <conditional statement> | <repetitive statement> | <with statement>
// Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement
// IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]
// WhileStatement := "WHILE" Expression "DO" Statement
// ForStatement := "FOR" AssignementStatement "To" Expression "DO" Statement
// CaseStatement := case <expression> of <case list element> {; <case list element> } end
// CaseListElement := <case label list> : <statement> | <empty>
// CaseLabelList := <constant> {, <constant> }
// CompoundStatement := "BEGIN" Statement { ";" Statement } "END"
// AssignementStatement := Letter ":=" Expression
// DisplayStatement := "DISPLAY" Expression
// FunctionDeclaration ::= <function heading> <block>
// FunctionHeading ::= function <identifier> : <result type> ; | function <identifier> ( <formal parameter section> {;<formal parameter section>} ) : <result type> ;
// FormalParameterSection ::= <parameter group> | var <parameter group> |
// ParameterGroup ::= <identifier> {, <identifier>} : <type identifier>


// Expression := SimpleExpression [RelationalOperator SimpleExpression]
// SimpleExpression := Term {AdditiveOperator Term}
// Term := Factor {MultiplicativeOperator Factor}
// Factor := Number | Letter | ID | "(" Expression ")"| "!" Factor
// Number := Digit{Digit}

// AdditiveOperator := "+" | "-" | "||"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// Letter := "a"|...|"z"
	
TYPE Identifier(void) {
	string name = lexer->YYText();
	TYPE type = VariablesWithTypes[name];

	switch (type) {
		case TYPE_UNSIGNED_INT:
			cout << "\tmov " << name << "(%rip), %rax\t# Load integer variable" << endl;
			cout << "\tpush %rax" << endl;
			break;

		case TYPE_BOOLEAN:
		case TYPE_CHAR:
			cout << "\tmovzbl " << name << "(%rip), %eax\t# Load byte (bool/char)" << endl;
			cout << "\tpush %rax" << endl;
			break;

		case TYPE_DOUBLE:
			cout << "\tmovsd " << name << "(%rip), %xmm0\t# Load double variable" << endl;
			cout << "\tsubq $8, %rsp" << endl;
			cout << "\tmovsd %xmm0, (%rsp)" << endl;


			break;

		default:
			Error("Type de variable non supporté dans Identifier()");
	}

	current = (TOKEN) lexer->yylex();
	return type;
}

TYPE Number(void) {
	string txt = lexer->YYText();
	double d;					// 64-bit float
	unsigned int *i;			// pointer to a 32 bit unsigned int 

	if (txt.find('.') != string::npos) {
		// FLOAT constant handling
		d=atof(lexer->YYText());
		i=(unsigned int *) &d; // i points to the const double
		//cout <<"\tpush $"<<*i<<"\t# Conversion of "<<d<<endl;
		// Is equivalent to : 
		cout <<"\tsubq $8,%rsp\t\t\t# allocate 8 bytes on stack's top"<<endl;
		cout <<"\tmovl	$"<<*i<<", (%rsp)\t# Conversion of "<<d<<" (32 bit high part)"<<endl;
		cout <<"\tmovl	$"<<*(i+1)<<", 4(%rsp)\t# Conversion of "<<d<<" (32 bit low part)"<<endl;

		current = (TOKEN) lexer->yylex();
		return TYPE_DOUBLE;
	} else {
		// INTEGER constant handling
		cout <<"\tpush $"<<atoi(lexer->YYText())<<endl;
		current = (TOKEN) lexer->yylex();
		return TYPE_UNSIGNED_INT;
	}
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
void VarDeclaration(void);
void StructuredStatement(void);
void CaseListElement(TYPE, int);
TYPE CaseLabelList(TYPE, int);

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
	else if (current == FLOATCONST) {
		return Number();
	}
	else if (current == CHARCONST) {
		return CharConst();
	}
	else if (current == ID) {
		return Identifier();
	} else if (current == NOT) {
		current = (TOKEN) lexer->yylex();
		TYPE t = Factor();
		if (t != TYPE_BOOLEAN)
			TypeError("L'opérateur '!' ne peut s'appliquer qu'à un booléen");

		cout << "\tpop %rax" << endl;
		cout << "\tnot %rax" << endl;
		cout << "\tpush %rax" << endl;
		return TYPE_BOOLEAN;
    } else {
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
	current = (TOKEN) lexer->yylex();  // saute le mot-clé VAR
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
string AssignementStatement(void) {
	string variable;
	if (current != ID)
		Error("Identificateur attendu");

	if (!IsDeclared(lexer->YYText())) {
		cerr << "Erreur : Variable '" << lexer->YYText() << "' non déclarée" << endl;
		exit(-1);
	}

	variable = lexer->YYText();
	TYPE type1 = VariablesWithTypes[variable];

	current = (TOKEN) lexer->yylex();
	if (current != ASSIGN)
		Error("caractères '=' attendus");

	current = (TOKEN) lexer->yylex();
	TYPE type2 = Expression();

	// Allow assignment from numeric to boolean if value is 0 or 1
if (type1 == TYPE_BOOLEAN && (type2 == TYPE_UNSIGNED_INT || type2 == TYPE_DOUBLE)) {
		string exprText = lexer->YYText(); // Get raw token text

		if (exprText == "0" || exprText == "1" || exprText == "0.0" || exprText == "1.0") {
			cout << "\t# Conversion numérique vers booléen" << endl;
			cout << "\tcmp $0, %rax" << endl;           // compare with zero
			cout << "\tsete %al" << endl;               // set %al = 1 if equal, 0 otherwise
			cout << "\tmovzbq %al, %rax" << endl;       // zero-extend to 64-bit
		} else {
			TypeError("Affectation invalide : valeur numérique non booléenne assignée à '" + variable + "'");
		}
	}
	else if (type1 != type2) {
		TypeError("Affectation invalide : la variable '" + variable +
				  "' est de type " + TypeToString(type1) +
				  ", mais l'expression est de type " + TypeToString(type2));
	}

	if (type1 == TYPE_UNSIGNED_INT) {
		cout << "\tpop %rax" << endl;
		cout << "\tmov %rax, " << variable << "(%rip)" << endl;
	}
	else if (type1 == TYPE_BOOLEAN || type1 == TYPE_CHAR) {
		cout << "\tpop %rax" << endl;
		cout << "\tmovb %al, " << variable << "(%rip)" << endl;
	}
	else if (type1 == TYPE_DOUBLE) {
		cout << "\tmovsd (%rsp), %xmm0" << endl;
		cout << "\taddq $8, %rsp" << endl;
		cout << "\tmovsd %xmm0, " << variable << "(%rip)" << endl;
	} else {
	Error("Type inconnu pour l'affectation dans AssignementStatement()");
	}

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
	StructuredStatement();

	cout << "\tjmp Suite" << ifTag << endl;

	cout << "Else" << ifTag << ":" << endl;
	if (current == ELSE) {
		current = (TOKEN) lexer->yylex();
		StructuredStatement();
	}
	cout << "Suite" << ifTag << ":" << endl;
}

// WhileStatement := "WHILE" Expression "DO" Statement
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
	StructuredStatement();  // body of the loop

	// Jump back to start
	cout << "\tjmp While" << whileTag << endl;

	// End label
	cout << "EndWhile" << whileTag << ":" << endl;
}

// RepeatStatement ::= repeat <statement> {; <statement>} until <expression>
void RepeatStatement(void){
	int repeatTag = ++TagNumber;

	// Label for start of loop
	cout << "Repeat" << repeatTag << ":" << endl;

	current = (TOKEN) lexer->yylex(); // read after 'REPEAT'

	// Parse at least one statement
	StructuredStatement();
	while(current == SEMICOLON){
		current = (TOKEN) lexer->yylex();

		// Ne pas exécuter StructuredStatement() si on atteint END après ;
		if (current == UNTIL) break;

		StructuredStatement();
	}


	// Check for UNTIL
	if (current != UNTIL)
		Error("Expected 'UNTIL' after repeat block");

	current = (TOKEN) lexer->yylex(); // read after 'UNTIL'

	TYPE TypeCondition = Expression();
	if (TypeCondition != TYPE_BOOLEAN)
		TypeError("Condition in REPEAT UNTIL must be BOOLEAN");

	cout << "\tpop %rax" << endl;
	cout << "\tcmp $0, %rax" << endl;
	cout << "\tje Repeat" << repeatTag << "\t# Repeat if condition is false" << endl;

	// End label (optional, but good style)
	cout << "EndRepeat" << repeatTag << ":" << endl;
}

// ForStatement := "FOR" AssignementStatement "To" Expression "DO" Statement
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
	StructuredStatement();  // loop body

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
void CompoundStatement(void){
	current = (TOKEN) lexer->yylex(); 
	StructuredStatement();

	while(current == SEMICOLON){
		current = (TOKEN) lexer->yylex();

		// Ne pas exécuter StructuredStatement() si on atteint END après ;
		if (current == END) break;

		StructuredStatement();
	}


	if (current != END){
	cerr << "[DEBUG] current token = '" << lexer->YYText() << "' (" << current << ")" << endl;		Error("Expected 'END'");
	}
	current = (TOKEN) lexer->yylex();
}

void CaseStatement(void){
	int CaseTag = ++TagNumber;

	current = (TOKEN) lexer->yylex(); // read after 'CASE'

	TYPE TypeExpression = Expression();

	if (TypeExpression != TYPE_CHAR && TypeExpression != TYPE_UNSIGNED_INT && TypeExpression != TYPE_DOUBLE)
		TypeError("Expression invalide dans la Case : type autorisé CHAR, INTEGER ou DOUBLE");

	//current = (TOKEN) lexer->yylex(); // read OF
	if (current != OF)
		Error("Expected 'OF' for the case statement!");

	current = (TOKEN) lexer->yylex(); // read after 'OF'

	// we pop according to the type of the expression
	if (TypeExpression == TYPE_DOUBLE) {
		cout << "\tmovsd (%rsp), %xmm0\t# Load double value for comparison" << endl;
		cout << "\taddq $8, %rsp" << endl;
	} else if (TypeExpression == TYPE_CHAR) {
		cout << "\tpop %rax\t\t# Pop CHAR into rax" << endl;
		cout << "\tmov %al, %bl\t\t# Save char in bl for comparison" << endl;
	} else {
		cout << "\tpop %rax\t\t# Pop INTEGER or BOOLEAN into rax" << endl;
	}

	CaseListElement(TypeExpression, CaseTag);


	while (current == SEMICOLON) {
		current = (TOKEN) lexer->yylex();
		if (current == END) break;
		CaseListElement(TypeExpression, CaseTag);
	}

	if (current != END)
		Error("Expected 'END' to close the CASE block");

	// 🔹 Add this label for correct branching:
	cout << "EndCase" << CaseTag << ":" << endl;

	current = (TOKEN) lexer->yylex(); // read after 'END'
}

void CaseListElement(TYPE TypeExpression, int caseEndLabel){
	int caseLabel = ++TagNumber;

	CaseLabelList(TypeExpression, caseLabel);

	if (current != COLON)
		Error("Expected ':' after case label list");
	current = (TOKEN) lexer->yylex();

	cout << "CaseMatch" << caseLabel << ":" << endl;

	if (current != SEMICOLON) {
		StructuredStatement();
		cout << "\tjmp EndCase" << caseEndLabel << endl;
	}

	cout << "Skip" << caseLabel << ":" << endl;
}

TYPE CaseLabelList(TYPE TypeExpression, int labelTarget){
	TYPE TypeConstant = TYPE_UNDEFINED;

	if (current != NUMBER && current != CHARCONST && current != FLOATCONST)
		Error("Case label must be a number, character, or float.");

	while (true) {
		int value;
		if (current == NUMBER) {
			value = stoi(lexer->YYText());
			TypeConstant = TYPE_UNSIGNED_INT;
		}
		else if (current == CHARCONST) {
			value = (int)lexer->YYText()[1];
			TypeConstant = TYPE_CHAR;
		} 
		else if (current == FLOATCONST) {
			value = stod(lexer->YYText());
			TypeConstant = TYPE_DOUBLE;
		}

		if (TypeExpression != TypeConstant)
			Error("Mismatched types between case expression and label.");

		// Decide which register to use
		string reg = (TypeExpression == TYPE_CHAR) ? "%bl" : "%rax";

		cout << "\tcmp $" << value << ", " << reg << "\t# Compare with case label" << endl;
		cout << "\tje CaseMatch" << labelTarget << endl;

		current = (TOKEN) lexer->yylex();

		if (current != COMMA) break; // no more constants
		current = (TOKEN) lexer->yylex(); // skip comma
	}

	cout << "\tjmp Skip" << labelTarget << "\t# No match, skip this case block" << endl;

	return TypeConstant;
}

void DisplayStatement(void) {
    current = (TOKEN)lexer->yylex();
    TYPE ExprType = Expression();

    cout << "\t# Affichage de type: " << TypeToString(ExprType) << endl;

    if (ExprType == TYPE_UNSIGNED_INT || ExprType == TYPE_BOOLEAN) {
        // integer / boolean: printf("%llu\n", rax)
        cout << "\tpop    %rax                     # integer to print\n";
        cout << "\tmov    %rax, %rsi               # 2nd arg → RSI\n";
        cout << "\tleaq   FormatString1(%rip), %rdi# 1st arg → RDI\n";
        cout << "\tmovl   $0, %eax                  # no SSE regs\n";
        cout << "\tcall   printf@PLT\n";

    } else if (ExprType == TYPE_DOUBLE) {
        // double: printf("%f\n", xmm0)
        cout << "\tmovsd  (%rsp), %xmm0            # load double from stack\n";
        cout << "\taddq   $8, %rsp                 # pop it\n";
        cout << "\tsubq   $8, %rsp                 # align stack (RSP%16==8)\n";
        cout << "\tleaq   FormatString2(%rip), %rdi# format string → RDI\n";
        cout << "\tmovl   $1, %eax                  # one SSE-reg in varargs\n";
        cout << "\tcall   printf@PLT\n";
        cout << "\taddq   $8, %rsp                 # restore stack\n";

    } else if (ExprType == TYPE_CHAR) {
        // char: printf("%c\n", al)
        cout << "\tpop    %rax                     # char in AL\n";
        cout << "\tmovb   %al, %sil                # 2nd arg (char) → SIL\n";
        cout << "\tleaq   FormatString3(%rip), %rdi# format → RDI\n";
        cout << "\tmovl   $0, %eax                  # no SSE regs\n";
        cout << "\tcall   printf@PLT\n";

    } else {
        Error("DISPLAY ne supporte que les types INTEGER, DOUBLE et CHAR.");
    }
}




// <conditional statement> ::= <if statement> | <case statement>
void ConditionalStatement(void){
	if (current == IF)
	{
		IfStatement();
	} else if (current == CASE)
	{
		CaseStatement();
	}
}

// RepetitiveStatement::= <while statement> | <repeat statemant> | <for statement>
void RepetitiveStatement(void){
	if (current == WHILE){
		WhileStatement();
	} else if (current == REPEAT)
	{
		RepeatStatement();
	} else if (current == FOR)
	{
		ForStatement();
	}
	
	
}

// WithStatement ::= with <record variable list> do <statement>
void WithStatement() {
    Error("WITH statement requires RECORD types, which are not supported yet.");
}

// <structured statement> ::= <compound statement> | <conditional statement> | <repetitive statement> | <with statement>
void StructuredStatement(void){
	if (current == BEGIN0)
	{
		CompoundStatement();
	} else if (current == ID)
	{
		AssignementStatement();
	} else if ((current == IF) || (current == CASE) )
	{
		ConditionalStatement();
	} else if ((current == REPEAT) || (current == WHILE)|| (current == FOR))
	{
		RepetitiveStatement();
	} else if (current == WITH)
	{
	 	WithStatement();
	} else if (current == DISPLAY)
	{
		DisplayStatement();
	}  else {
		cerr << "[DEBUG] current token = " << current << endl;
		Error("A weird input!");
	}

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
	} else if (current == CASE)
	{
		CaseStatement();
	}
	else if (current == BEGIN0)
	{
		CompoundStatement();
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
	StructuredStatement();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();
		StructuredStatement();
	}
	if(current!=DOT)
		Error("caractère '.' attendu");
	current=(TOKEN) lexer->yylex();
}

// ParameterGroup ::= <identifier> {, <identifier>} : <type identifier>
void ParameterGroup(void){
	if (current != ID)
	{
		Error("Expected an identifier in the parameter group");
	}
	// Collect parameter identifiers for comment output
	vector<string> paramNames;
	paramNames.push_back(lexer->YYText());
	current = (TOKEN)lexer->yylex();
	while (current == COMMA)
	{
		current = (TOKEN)lexer->yylex();
		if (current != ID)
		{
			Error("Expected an identifier in the parameter group after ','");
		}
		paramNames.push_back(lexer->YYText());
		current = (TOKEN)lexer->yylex();
	}

	if (current != COLON)
	{
		Error("Expected a colon after the parameter group");
	}
	current = (TOKEN)lexer->yylex();

	TYPE type = Type(); // Validate and consume type identifier
	// Output a comment in assembly for each parameter group
	cout << "\t# Parameter group parsed with type: " << TypeToString(type) << endl;
}

// FormalParameterSection ::= <parameter group> | var <parameter group> |
void FormalParameterSection(void){
	if (current == VAR)
	{
		current=(TOKEN) lexer->yylex();
		ParameterGroup();
	} else {
		ParameterGroup();
	}
	
}

// FunctionHeading ::= function <identifier> : <result type> ; | function <identifier> ( <formal parameter section> {;<formal parameter section>} ) : <result type> ;
void FunctionHeading(void){
	if (current != FUNCTION)
	{
		Error("Expected 'function'");
	}

	current=(TOKEN) lexer->yylex();

	if (current != ID)
	{
		Error("Expected an identifier after 'function'");
	}
	// Output function heading comments and label
	string funcName = lexer->YYText();  // Sauvegarder le nom avant de passer au token suivant
	cout << "\t.globl " << funcName << "\n";
	cout << funcName << ":\n";
	current=(TOKEN) lexer->yylex();	
	if (current == LPARENT)
	{
		current = (TOKEN) lexer->yylex();
		FormalParameterSection();
		while (current == SEMICOLON )
		{
			current = (TOKEN) lexer->yylex();
			FormalParameterSection();
		}
		if (current != RPARENT)
		{
			Error("Expected ')' after parameter list");
		}
		current = (TOKEN) lexer->yylex();
	}

	// now check for the colon
	if (current != COLON)
	{
		Error("Expected ':' after function name or after the list of parameters");
	}
	current = (TOKEN) lexer->yylex();

	TYPE returnType = Type();  // capture the return type
	VariablesWithTypes[funcName] = returnType;  // treat function name as variable

	if (current != SEMICOLON)
	{
		Error("Expected ';' at the end of function heading");
	}
	current = (TOKEN)lexer->yylex();
	cout << "\t# Function return type parsed\n";
}

// FunctionDeclaration ::= <function heading> <block>
void FunctionDeclaration(void){
	FunctionHeading();
	cout << "\tpush %rbp\t# Save base pointer" << endl;
	cout << "\tmov %rsp, %rbp\t# Set new base pointer" << endl;
	StatementPart();
	cout << "\tmov %rbp, %rsp\t# Restore stack before returning" << endl;
	cout << "\tpop %rbp\t# Restore base pointer" << endl;
	cout << "\tret\t# Return from function" << endl;
}

// Program := [DeclarationPart] StatementPart
void Program(void){
	if(current==VAR)
		VarDeclarationPart();

	while (current == FUNCTION) {
		FunctionDeclaration(); // 🔧 appelle ta fonction existante
	}

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
	cout << "\t.section .note.GNU-stack,\"\",@progbits" << endl;
	if(current!=FEOF){
		cerr <<"Caractères en trop à la fin du programme : ["<<current<<"]";
		Error("."); // unexpected characters at the end of program
	}

}