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

using namespace std;

char current, lookedAhead;                // Current char    
int NLookedAhead=0;

void ReadChar(void){
    if(NLookedAhead>0){
        current=lookedAhead;    // Char has already been read
        NLookedAhead--;
    }
    else
        // Read character and skip spaces until 
        // non space character is read
        while(cin.get(current) && (current==' '||current=='\t'||current=='\n'));
}

void LookAhead(void){
    while(cin.get(lookedAhead) && (lookedAhead==' '||lookedAhead=='\t'||lookedAhead=='\n'));
    NLookedAhead++;
}

void Error(string s){
	cerr<< s << endl;
	exit(-1);
}

// Program := [DeclarationPart] StatementPart
// DeclarationPart := "[" Letter {"," Letter} "]"
// StatementPart := Statement {";" Statement} "."
// Statement := AssignementStatement
// AssignementStatement := Letter "=" Expression

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
// SimpleExpression := Term {AdditiveOperator Term}
// Term := Factor {MultiplicativeOperator Factor}
// Factor := Number | Letter | "(" Expression ")"| "!" Factor
// Number := Digit{Digit}

// AdditiveOperator := "+" | "-" | "||"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// Letter := "a"|...|"z"


void Number(void){
	if((current<'0')||(current>'9'))
		Error("Chiffre attendu");		   // Digit expected
	else{
		int value = 0;
		while ((current>='0')&&(current<='9'))
		{
            value = value * 10 + (current - '0'); // Construire le nombre
			ReadChar();
		}
		cout << "\tpush $"<<value<<endl;
	}
}

void Term(void){
	Factor();

	while ((current=='*') || (current=='/') || (current=='%')|| (current=='&'))
	{
		MultiplicativeOperator();
		Factor();
	}
}

void Factor(void){
	if (isdigit(current))
	{
		Number();
	} else if (current >= 'a' && current <= 'z')
	{
		cout << "\tpush "<<current<<endl;			// push the letter
		ReadChar();
	} else if (current == '(')
	{
		ReadChar();        // pour sauter '('
		Expression();      // on lit l'expression entre parenthèses
		if (current != ')')
			Error("Expected ')'");
		ReadChar();        // pour sauter ')'
	} else if (current == '!')
	{
		Factor();
		cout << "\tpop %rax"<<endl;			
		cout << "\tcmp $0, %rax"<<endl;		
		cout << "\tsete %al"<<endl;			
		cout << "\tmovzbq %al, %rax"<<endl;			
		cout << "\tpush %rax"<<endl;			
	}
}

void MultiplicativeOperator(void){
	cout << "\tpop %rbx"<<endl;			
	cout << "\tpop %rax"<<endl;			

	switch (current)
	{
		case '&':
			char adop = current;
			char nextadop;
			LookAhead();
			if ((lookedAhead == '&'))
			{
				nextadop = lookedAhead;
				NLookedAhead--;
			} else
			{
				Error("Expected '&");
			}
			cout << "\tcmp $0, %rax"<<endl;	
			cout << "\tsetne %al"<<endl;			
			cout << "\tcmp $0, %rbx"<<endl;			
			cout << "\tsetne %bl"<<endl;			
			cout << "\tandb %bl, %al"<<endl;			
			cout << "\tmovzbq %al, %rax"<<endl;	
			cout << "\tpush %rax"<<endl;	
			break;

		case '*':
			cout << "\timulq %rbx, %rax"<<endl;	
			cout << "\tpush %rax" << endl;		
			break;

		case '/':
			cout << "\tcqto" << endl;
			cout << "\tidivq %rbx"<<endl;	
			cout << "\tpush %rax"<<endl;					
			break;

		case '%':
			cout << "\tcqto"<<endl;		
			cout << "\tidivq %rbx"<<endl;	
			cout << "\tpush %rdx"<<endl;					
			break;

		default:
			break;
	}
	ReadChar();
}

void DeclarationPart(void){
    ReadChar();
    if (current == '[') {
        cout << ".data" << endl;
        do {
            ReadChar(); // Lire la lettre
            if (current >= 'a' && current <= 'z') {
                cout << current << ":\t.quad 0" << endl;
            } else {
                Error("Letter expected!");
            }
            ReadChar(); // Lire la virgule ou le ']'
        } while (current == ',');  // continuer si on a une virgule

        if (current != ']') {
            Error("Expected ']'");
        }
    }
}

void StatementPart(void){
	Statement();
	while (current == ';')
	{
		Statement();
	}

	if (current != '.')
	{
		Error("Expected '.'");
	}
	
}

void Statement(void){
	ReadChar();
	AssignementStatement();
}

void AssignementStatement(void){
	if (!(current >= 'a' && current <= 'z')) Error("Expected variable name");
	char var = current;
	
	ReadChar();
	if (current != '=') Error("Expected '='");
	
	ReadChar(); // avancer sur la première lettre/chiffre de l’expression
	Expression();
	
	cout << "\tpop %rax" << endl;
	cout << "\tmov %rax, " << var << endl;
}

void Expression(void){
	SimpleExpression();
	if (current != ';' && current != '.')
	{
		RelationalOperator();
		SimpleExpression();
	}
}

void RelationalOperator(void){
	cout << "\tpop %rbx" << endl; // Deuxième opérande
	cout << "\tpop %rax" << endl; // Première opérande

	char adop = current; // opérateur principal : =, <, >, !
	char nextadop = '\0';

	// Lire le 2e caractère pour tester ==, !=, <=, >=
	LookAhead();
	if (lookedAhead == '=' || lookedAhead == '>')
	{
		nextadop = lookedAhead;
		NLookedAhead--;
	}

	switch (adop)
	{
		case '=':
			if (nextadop == '=')
			{
				cout << "\tcmp %rbx, %rax" << endl;
				cout << "\tsete %al" << endl;
			}
			else
				Error("Expected '=' after '='");
			break;

		case '!':
			if (nextadop == '=')
			{
				cout << "\tcmp %rbx, %rax" << endl;
				cout << "\tsetne %al" << endl;
			}
			else
				Error("Expected '=' after '!'");
			break;

		case '<':
			if (nextadop == '=')
			{
				cout << "\tcmp %rbx, %rax" << endl;
				cout << "\tsetle %al" << endl;
			}
			else
			{
				cout << "\tcmp %rbx, %rax" << endl;
				cout << "\tsetl %al" << endl;
			}
			break;

		case '>':
			if (nextadop == '=')
			{
				cout << "\tcmp %rbx, %rax" << endl;
				cout << "\tsetge %al" << endl;
			}
			else
			{
				cout << "\tcmp %rbx, %rax" << endl;
				cout << "\tsetg %al" << endl;
			}
			break;

		default:
			Error("Unknown relational operator");
	}

	cout << "\tmovzbq %al, %rax" << endl; // étendre %al (8 bits) en %rax (64 bits)
	cout << "\tpush %rax" << endl;        // résultat logique (0 ou 1) empilé
	ReadChar();
}

void SimpleExpression(void){
	Term();
	while ((current=='+') || (current=='-') || (current=='|'))
	{
		AdditiveOperator();
		Term();
	}
}

void AdditiveOperator(void){
	cout << "\tpop %rbx"<<endl;			
	cout << "\tpop %rax"<<endl;			
	switch (current)
	{
		case '|':
			char adop = current;
			char nextadop;
			LookAhead();
			if ((lookedAhead == '|'))
			{
				nextadop = lookedAhead;
				NLookedAhead--;
			} else
			{
				Error("Expected '|");
			}
			cout << "\tcmp $0, %rax"<<endl;	
			cout << "\tsetne %al"<<endl;			
			cout << "\tcmp $0, %rbx"<<endl;			
			cout << "\tsetne %bl"<<endl;			
			cout << "\torb %bl, %al"<<endl;			
			cout << "\tmovzbq %al, %rax"<<endl;			
			break;

		case '+':
			cout << "\taddq %rbx, %rax"<<endl;		
			break;

		case '-':
			cout << "\tsubq %rbx, %rax"<<endl;		
			break;

		default:
			break;
	}
	cout << "\tpush %rax"<<endl;	
	ReadChar();
}


int main(void){	// First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	cout << "\t\t\t# This code was produced by the CERI Compiler"<<endl;
	cout << "\t.text\t\t# The following lines contain the program"<<endl;
	cout << "\t.globl main\t# The main function must be visible from outside"<<endl;
	cout << "main:\t\t\t# The main function body :"<<endl;
	cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top"<<endl;

	// Let's proceed to the analysis and code production
	DeclarationPart();
	StatementPart();
	// Trailer for the gcc assembler / linker
	cout << "\tmovq %rbp, %rsp\t\t# Restore the position of the stack's top"<<endl;
	cout << "\tret\t\t\t# Return from main function"<<endl;
	if(cin.get(current)){
		cerr <<"Caractères en trop à la fin du programme : ["<<current<<"]";
		Error("."); // unexpected characters at the end of program
	}

}
		
			





