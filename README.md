# CERIcompiler

A simple compiler.
From : Pascal-like imperative LL(k) langage
To : 64 bit 80x86 assembly langage (AT&T)

**Download the repository :**

> git clone git@framagit.org:jourlin/cericompiler.git

**Build the compiler with debug symbols :**

> g++ -ggdb compilateur.cpp -o compilateur

**Compile the test program :**

> cat test.p | ./compilateur > test.s

**Have a look at the output :**

> gedit test.s

**Produce the executable (with debug info) :**

> gcc -ggdb test.s -o test :

**Debug the executable :**

> ddd ./test

**Commit the new version :**

> git commit -a -m "What's new..."

**Send to your framagit :**

> git push -u origin master

**Get from your framagit :**

> git pull -u origin master


# MonCompilateur README

**A compiler from a simple Pascal-like language to x86-64 assembly**  
**Author:** Zineb Meftah .  
**Date:** June 2025  

---

## Overview

This compiler translates programs written in a minimal, Pascal-inspired language into 64-bit x86-64 assembly (AT&T syntax). It supports:

1. **Basic types**:  
   - **INTEGER** (32-bit unsigned)  
   - **DOUBLE** (64-bit IEEE-754)  
   - **BOOLEAN** (1-byte, 0 = FALSE, 0xFF = TRUE)  
   - **CHAR** (1-byte ASCII)  
   - **STRING** (pointer to a null-terminated ASCII sequence)  

2. **Global variables**: All variables (“`VAR … .`” section) are global; no local `VAR` declarations inside procedures or functions.  

3. **Arithmetic expressions**:  
   - Integer arithmetic (`+`, `-`, `*`, `/`, `%`)  
   - Mixed INTEGER ↔ DOUBLE arithmetic with automatic promotion  
   - Boolean operators (`&&`, `||`, `!`)  
   - Relational operators (`==`, `!=`, `<`, `>`, `<=`, `>=`)  

4. **Strings**:  
   - String literals.  
   - String concatenation via a runtime helper `__concatStrings`.  
   - Concatenation of any two `STRING` values produces a newly allocated string; the address is stored in a temporary slot (`tempStringSlot`).  
   - Empty string concatenations (`"" + s`) work as expected.  

5. **Control structures**:  
   - **IF … THEN … ELSE**  
   - **WHILE … DO**  
   - **REPEAT … UNTIL**  
   - **FOR var := start TO end [STEP n] DO**  
   - **CASE … OF … END**  

6. **Procedures/Functions**:  
   - Procedure definitions (`PROCEDURE Name; BEGIN … END;`).  
   - Nested/hierarchical calls: procedures can call other procedures (e.g., `DeepNested` calls `BuildMessageExtended`).  
   - No parameters or return values (aside from global‐variable side-effects).  

7. **DISPLAY statements**:  
   - `DISPLAY(expr)` prints an expression of type `INTEGER`, `DOUBLE`, `BOOLEAN`, `CHAR`, or `STRING` to stdout.  
   - All formatting is done via `printf@PLT` using appropriate format strings (`"%llu\n"`, `"%f\n"`, `"%c\n"`, `"%s\n"`).  
   - Booleans are collapsed to `0 or 1` before printing.  

---

## Compiler internals

### 1. Lexer & Tokenizer

- We use **Flex** to tokenize the source.  
- Tokens include keywords (`VAR`, `BEGIN`, `END`, `PROCEDURE`, `DISPLAY`, etc.), operators (`+`, `-`, `*`, `/`, `%`, `&&`, `||`, `==`, `!=`, `<`, `>`, `<=`, `>=`), identifiers, numeric literals (integer & floating), character literals (`'a'`), and string literals (`"hello"`).  
- Each token has an associated enum `TOKEN` (e.g., `ID`, `NUMBER`, `FLOATCONST`, `CHARCONST`, `STRINGCONST`, `TRUE0`, `FALSE0`, `ADDOP`, `MULOP`, `RELOP`, etc.).


```cpp
// Example snippet from tokeniser-related code
enum TOKEN { NUMBER, FLOATCONST, CHARCONST, STRINGCONST, TRUE0, FALSE0, ID, ADDOP, MULOP, RELOP, DISPLAY, ASSIGN, ... };
```

### 2. Symbol‐table & Type‐checking

- A global map `VariablesWithTypes` stores each declared variable’s type (`TYPE_UNSIGNED_INT`, `TYPE_BOOLEAN`, `TYPE_CHAR`, `TYPE_DOUBLE`, `TYPE_STRING`).  
- On each declaration (`VAR i, j : INTEGER;`), the compiler emits the corresponding `.quad 0`, `.byte 0`, or `.double 0.0` directives.  
- When an identifier is encountered in an expression, we look up its type. If undeclared, we raise an error.  
- **CheckBinaryOperationTypes(t1, t2, op)** ensures type‐compatibility for `t1 op t2`:  
  - INTEGER ↔ INTEGER → INTEGER  
  - DOUBLE ↔ DOUBLE → DOUBLE  
  - INTEGER ↔ DOUBLE or DOUBLE ↔ INTEGER → DOUBLE (with implicit promotion)  
  - BOOLEAN only with BOOLEAN for `&&`, `||`, `==`, `!=`  
  - `%` only on two INTEGERs  
  - Relational operators on numeric types produce a BOOLEAN.  


```cpp
TYPE CheckBinaryOperationTypes(TYPE t1, TYPE t2, string op) {
    if ((t1 == TYPE_UNSIGNED_INT && t2 == TYPE_DOUBLE) || (t1 == TYPE_DOUBLE && t2 == TYPE_UNSIGNED_INT))
        return TYPE_DOUBLE;
    if (t1 == t2)
        return t1;
    // additional checks...
    TypeError("Types incompatibles pour '" + op + "' entre " + TypeToString(t1) + " et " + TypeToString(t2));
    return TYPE_UNDEFINED; // not reached
}
```

### 3. Parsing & Code‐generation

The compiler uses a recursive‐descent parser. For each grammar rule, we emit assembly as we go (“one‐pass code generation”).

#### 3.1. Expressions

- **Factor**  
  - `(` Expression `)` → recursive call.  
  - `NUMBER` (lexeme with no '.') → push immediate integer.  
  - `FLOATCONST` (contains '.') → push 64-bit IEEE bits in two `movl` lines.  
  - `CHARCONST` → push a 1-byte ASCII code inside a 64-bit `rax`.  
  - `TRUE0` / `FALSE0` → push `$0xFFFFFFFFFFFFFFFF` or `$0`  
  - `STRINGCONST` → generate a new label `StrConstN` in `.rodata`, and push it.  


```cpp
TYPE Factor(void) {
    if (current == NUMBER) {
        // Integer constant
        cout << "\tpush $" << atoi(lexer->YYText()) << endl;
        current = (TOKEN) lexer->yylex();
        return TYPE_UNSIGNED_INT;
    } else if (current == FLOATCONST) {
        // Floating constant
        double d = atof(lexer->YYText());
        unsigned int* i = (unsigned int*)&d;
        cout << "\tsubq $8,%rsp\t# allocate 8 bytes\n";
        cout << "\tmovl $" << *i << ", (%rsp)\t# high part of " << d << "\n";
        cout << "\tmovl $" << *(i+1) << ", 4(%rsp)\t# low part of " << d << "\n";
        current = (TOKEN) lexer->yylex();
        return TYPE_DOUBLE;
    }
    // ... other cases ...
}
```

- **Term** (handles `*`, `/`, `%`, `&&`)  
  1. Parse left Factor → `type1`.  
  2. While next token is `MULOP`:  
     - Record `mulop` (`MUL`, `DIV`, `MOD`, `AND`).  
     - Parse right Factor → `type2`.  
     - Compute `commonType = CheckBinaryOperationTypes(type1, type2, op)`.  
     - **If** `commonType == DOUBLE`:  
       - Call `PromoteToDoubleIfNeeded(type2, 1)` and `PromoteToDoubleIfNeeded(type1, 2)`.  
       - Use x87 instructions (`fldl`, `fmulp`, `fdivp`), then store.  
     - **Else** (both are integers or booleans):  
       - Use integer instructions (`mulq %rbx`, `div %rbx`, `push %rdx` for `%`, etc.).  


```cpp
TYPE Term(void) {
    TYPE type1 = Factor();
    while (current == MULOP) {
        OPMUL mulop = MultiplicativeOperator();
        TYPE type2 = Factor();
        TYPE commonType = CheckBinaryOperationTypes(type1, type2, (mulop == MUL)? "*" : "/");
        if (commonType == TYPE_DOUBLE) {
            // Promote and use x87
            PromoteToDoubleIfNeeded(type2, 1);
            PromoteToDoubleIfNeeded(type1, 2);
            cout << "\tfldl  (%rsp)\n";
            cout << "\taddq $8, %rsp\n";
            cout << "\tfldl  (%rsp)\n";
            cout << "\taddq $8, %rsp\n";
            cout << "\tfmulp %st, %st(1)\n";
            cout << "\tsubq $8, %rsp\n";
            cout << "\tfstpl (%rsp)\n";
            type1 = commonType;
        } else {
            cout << "\tpop %rbx\n\tpop %rax\n";
            cout << "\tmulq %rbx\n\tpush %rax\t# MUL\n";
            type1 = commonType;
        }
    }
    return type1;
}
```

- **SimpleExpression** (handles `+`, `-`, `||`)  
  1. Parse left Term → `type1`.  
  2. While next token is `ADDOP`:  
     - Record `adop` (`ADD`, `SUB`, `OR`).  
     - Parse right Term → `type2`.  
     - Compute `commonType`.  
     - **If** both `TYPE_STRING` and `op = ADD` → string concatenation (pop, call `__concatStrings`, etc.).  
     - **Else if** `commonType == DOUBLE`:  
       - Promote operands, use x87 (`fldl`, `faddp`, `fsubp`), store result.  
     - **Else** (both INTEGER/BOOLEAN):  
       - Pop registers, integer `addq`, `subq`, `orq`, then `push`.  


```cpp
TYPE SimpleExpression(void) {
    TYPE type1 = Term();
    while (current == ADDOP) {
        OPADD adop = AdditiveOperator();
        TYPE type2 = Term();
        TYPE commonType = CheckBinaryOperationTypes(type1, type2, (adop == ADD) ? "+" : "-");
        if (type1 == TYPE_STRING && type2 == TYPE_STRING && adop == ADD) {
            cout << "\t# Concatenating string + string\n";
            cout << "\tpop %rsi\n\tpop %rdi\n";
            cout << "\tsub $8, %rsp\n\tcall __concatStrings\n";
            cout << "\tadd $8, %rsp\n";
            cout << "\tmov %rax, tempStringSlot(%rip)\n";
            cout << "\tmov tempStringSlot(%rip), %rax\n";
            cout << "\tpush %rax\n";
            type1 = commonType;
        } else if (commonType == TYPE_DOUBLE) {
            // Promote & use x87
            if (type2 == TYPE_UNSIGNED_INT) {
                cout << "\tpop %rax\n\tcvtsi2sd %rax, %xmm0\n";
                cout << "\tsubq $8, %rsp\n";
                cout << "\tmovsd %xmm0, (%rsp)\n";
            }
            cout << "\tfldl  (%rsp)\n\taddq $8, %rsp\n";
            if (type1 == TYPE_UNSIGNED_INT) {
                cout << "\tpop %rax\n\tcvtsi2sd %rax, %xmm0\n";
                cout << "\tsubq $8, %rsp\n";
                cout << "\tmovsd %xmm0, (%rsp)\n";
            }
            cout << "\tfldl  (%rsp)\n\taddq $8, %rsp\n";
            cout << "\tfaddp %st, %st(1)\n";
            cout << "\tsubq $8, %rsp\n";
            cout << "\tfstpl (%rsp)\n";
            type1 = commonType;
        } else {
            cout << "\tpop %rbx\n\tpop %rax\n";
            cout << "\taddq %rbx, %rax\n\tpush %rax\n";
            type1 = commonType;
        }
    }
    return type1;
}
```

- **Expression** (handles optional `RelOp` + second `SimpleExpression`):  
  1. Call `SimpleExpression()` → `type1`.  
  2. If next token is `RELOP`:  
     - Parse `RelationalOperator` → `oprel`.  
     - Parse `SimpleExpression()` → `type2`.  
     - Compute `commonType`.  
     - **If** `commonType == DOUBLE`:  
       - Use x87 to compare (`fldl`, `fucomip`).  
     - **Else**:  
       - Pop registers, `cmpq`.  
     - Emit conditional jumps (`je VraiN`, `jne VraiN`, etc.), push result as `0xFFFFFFFFFFFFFFFF` or `0`.  


```cpp
TYPE Expression(void) {
    TYPE type1 = SimpleExpression();
    if (current == RELOP) {
        OPREL oprel = RelationalOperator();
        TYPE type2 = SimpleExpression();
        TYPE commonType = CheckBinaryOperationTypes(type1, type2, "<");
        if (commonType == TYPE_DOUBLE) {
            cout << "\tfldl (%rsp)\n\taddq $8, %rsp\n";
            cout << "\tfldl (%rsp)\n\taddq $8, %rsp\n";
            cout << "\tfucomip %st(1), %st\n";
            cout << "\tfstp %st(0)\n";
        } else {
            cout << "\tpop %rax\n\tpop %rbx\n\tcmpq %rbx, %rax\n";
        }
        cout << "\tje Vrai" << tag << "\n";
        cout << "\tpush $0\n";
        cout << "\tjmp Suite" << tag << "\n";
        cout << "Vrai" << tag << ":\tpush $0xFFFFFFFFFFFFFFFF\n";
        cout << "Suite" << tag << ":\n";
        return TYPE_BOOLEAN;
    }
    return type1;
}
```

#### 3.2. Assignment & Variables

- **Identifier** (as an l-value or r-value):  
  - `TYPE_UNSIGNED_INT`: load with `mov var(%rip), %rax; push %rax`.  
  - `TYPE_BOOLEAN`/`TYPE_CHAR`: `movzbl var(%rip), %eax; push %rax`.  
  - `TYPE_DOUBLE`: `movsd var(%rip), %xmm0; subq $8, %rsp; movsd %xmm0, (%rsp)`.  
  - `TYPE_STRING`: `mov var(%rip), %rax; push %rax`.  

- **AssignmentStatement** (`x := Expression`):  
  1. Evaluate `Expression` → `type2`.  
  2. Let `type1 = VariablesWithTypes[x]`.  
  3. If `type1 == BOOLEAN` and `type2` is numeric, convert to 0/1.  
  4. If `type1 != type2` (not a Boolean conversion), error.  
  5. Store the popped value into memory depending on type:  
     - INTEGER: `pop %rax; mov %rax, x(%rip)`.  
     - BOOLEAN/CHAR: `pop %rax; movb %al, x(%rip)`.  
     - DOUBLE: `movsd (%rsp), %xmm0; addq $8, %rsp; movsd %xmm0, x(%rip)`.  
     - STRING: `pop %rax; mov %rax, x(%rip)`.  


```cpp
string AssignementStatement(void) {
    string varName = lexer->YYText();
    TYPE type1 = VariablesWithTypes[varName];
    current = (TOKEN)lexer->yylex(); // skip ID
    current = (TOKEN)lexer->yylex(); // skip ASSIGN
    TYPE type2 = Expression();
    if (type1 == TYPE_BOOLEAN && (type2 == TYPE_UNSIGNED_INT || type2 == TYPE_DOUBLE)) {
        cout << "\t# Conversion numérique vers booléen\n";
        cout << "\tcmp $0, %rax\n\tsete %al\n\tmovzbq %al, %rax\n";
    } else if (type1 != type2) {
        TypeError("Type mismatch in assignment");
    }
    if (type1 == TYPE_UNSIGNED_INT) {
        cout << "\tpop %rax\n\tmov %rax, " << varName << "(%rip)\n";
    } else if (type1 == TYPE_BOOLEAN || type1 == TYPE_CHAR) {
        cout << "\tpop %rax\n\tmovb %al, " << varName << "(%rip)\n";
    } else if (type1 == TYPE_DOUBLE) {
        cout << "\tmovsd (%rsp), %xmm0\n\taddq $8, %rsp\n\tmovsd %xmm0, " << varName << "(%rip)\n";
    } else {
        cout << "\tpop %rax\n\tmov %rax, " << varName << "(%rip)\n";
    }
    return varName;
}
```

#### 3.3. DisplayStatement

`DISPLAY(expr)` does:

1. Parse `Expression()` → `ExprType`.  
2. Emit comment `# Affichage de type: <TYPE>`.  
3. Depending on `ExprType`:  
   - **TYPE_UNSIGNED_INT**: pop and call `printf("%llu\n", rax)`.  
   - **TYPE_BOOLEAN**: pop, normalize `0xFF`/`0x00` to `0`/`1`, call `printf("%llu\n", rax)`.  
   - **TYPE_DOUBLE**: load from stack into `%xmm0`, pop, call `printf("%f\n", xmm0)`.  
   - **TYPE_CHAR**: pop into `%rax`, move `%al` to `%sil`, call `printf("%c\n", sil)`.  
   - **TYPE_STRING**: pop into `%rsi`, align stack, call `printf("%s\n", rsi)`, restore stack.  


```cpp
void DisplayStatement(void) {
    current = (TOKEN)lexer->yylex(); // skip DISPLAY
    TYPE ExprType = Expression();
    cout << "\t# Affichage de type: " << TypeToString(ExprType) << endl;
    if (ExprType == TYPE_UNSIGNED_INT) {
        cout << "\tpop %rax\n\tmov %rax, %rsi\n";
        cout << "\tleaq FormatUInt(%rip), %rdi\n\tmovl $0, %eax\n";
        cout << "\tcall printf@PLT\n";
    } else if (ExprType == TYPE_BOOLEAN) {
        cout << "\tpop %rax\n\ttestb %al, %al\n\tsetne %al\n\tmovzbq %al, %rax\n";
        cout << "\tmov %rax, %rsi\n\tleaq FormatUInt(%rip), %rdi\n\tmovl $0, %eax\n";
        cout << "\tcall printf@PLT\n";
    } else if (ExprType == TYPE_DOUBLE) {
        cout << "\tmovsd (%rsp), %xmm0\n\taddq $8, %rsp\n";
        cout << "\tleaq FormatString2(%rip), %rdi\n\tmovl $1, %eax\n";
        cout << "\tcall printf@PLT\n";
    } else if (ExprType == TYPE_CHAR) {
        cout << "\tpop %rax\n\tmovb %al, %sil\n";
        cout << "\tleaq FormatString3(%rip), %rdi\n\tmovl $0, %eax\n";
        cout << "\tcall printf@PLT\n";
    } else if (ExprType == TYPE_STRING) {
        cout << "\t# display string\n";
        cout << "\tpop %rsi\n\tsub $8, %rsp\n\tleaq FormatString(%rip), %rdi\n\tmovl $0, %eax\n";
        cout << "\tcall printf@PLT\n\tadd $8, %rsp\n";
    } else {
        Error("DISPLAY ne supporte que les types INTEGER, DOUBLE, CHAR, BOOLEAN, STRING.");
    }
}
```

#### 3.4. Conditional & Loops

1. **IF Expression THEN Stmt [ELSE Stmt]**:  
   - Evaluate condition → `TYPE_BOOLEAN`.  
   - `pop %rax; cmp $0, %rax; je ElseTag`.  
   - Generate “then” code, `jmp SuiteTag`, label `ElseTag:`, else code if present, label `SuiteTag:`.  


```cpp
void IfStatement(void) {
    current = (TOKEN) lexer->yylex(); // skip IF
    TYPE condType = Expression();
    if (condType != TYPE_BOOLEAN) TypeError("IF condition must be BOOLEAN");
    cout << "\tpop %rax\n\tcmp $0, %rax\n";
    cout << "\tje Else" << ifTag << "\n";
    current = (TOKEN) lexer->yylex(); // skip THEN
    StructuredStatement();
    cout << "\tjmp Suite" << ifTag << "\n";
    cout << "Else" << ifTag << ":\n";
    if (current == ELSE) {
        current = (TOKEN) lexer->yylex();
        StructuredStatement();
    }
    cout << "Suite" << ifTag << ":\n";
}
```

2. **WHILE Expression DO Stmt**:  
   - Label `WhileN:`, evaluate condition, `pop %rax; cmp $0, %rax; je EndWhileN`, generate body, `jmp WhileN`, label `EndWhileN:`.  


```cpp
void WhileStatement(void) {
    cout << "While" << whileTag << ":\n";
    current = (TOKEN) lexer->yylex(); // skip WHILE
    TYPE condType = Expression();
    if (condType != TYPE_BOOLEAN) TypeError("WHILE condition must be BOOLEAN");
    cout << "\tpop %rax\n\tcmp $0, %rax\n";
    cout << "\tje EndWhile" << whileTag << "\n";
    current = (TOKEN) lexer->yylex(); // skip DO
    StructuredStatement();
    cout << "\tjmp While" << whileTag << "\n";
    cout << "EndWhile" << whileTag << ":\n";
}
```

3. **REPEAT Stmt {; Stmt} UNTIL Expression**:  
   - Label `RepeatN:`, generate statements, expect `UNTIL`, evaluate condition, `pop %rax; cmp $0, %rax; je RepeatN`, label `EndRepeatN:`.  


```cpp
void RepeatStatement(void) {
    cout << "Repeat" << repeatTag << ":\n";
    current = (TOKEN) lexer->yylex(); // skip REPEAT
    StructuredStatement();
    while (current == SEMICOLON) {
        current = (TOKEN) lexer->yylex();
        if (current == UNTIL) break;
        StructuredStatement();
    }
    if (current != UNTIL) Error("Expected 'UNTIL'");
    current = (TOKEN) lexer->yylex(); // skip UNTIL
    TYPE condType = Expression();
    if (condType != TYPE_BOOLEAN) TypeError("REPEAT UNTIL condition must be BOOLEAN");
    cout << "\tpop %rax\n\tcmp $0, %rax\n";
    cout << "\tje Repeat" << repeatTag << "\n";
    cout << "EndRepeat" << repeatTag << ":\n";
}
```

4. **FOR Var := expr1 TO expr2 [STEP n] DO Stmt**:  
   - Initialize loop variable, evaluate `expr2`, store in `%rbx`, optional `STEP`, label `TestForN:`, compare `var` with `%rbx`, `jg EndForN`, label `LoopForN:`, generate body, increment `var` by `step`, `jmp TestForN`, label `EndForN:`.  


```cpp
void ForStatement(void) {
    cout << "\t#FOR LOOP STARTED\n";
    current = (TOKEN) lexer->yylex(); // skip FOR
    string var = AssignementStatement();
    TYPE loopVarType = VariablesWithTypes[var];
    if (loopVarType != TYPE_UNSIGNED_INT) TypeError("FOR variable must be INTEGER");
    cout << "\tmov " << var << "(%rip), %rax\n";
    if (current != TO) Error("Expected 'TO'");
    current = (TOKEN) lexer->yylex();
    TYPE toType = Expression();
    if (toType != TYPE_UNSIGNED_INT) TypeError("FOR limit must be INTEGER");
    cout << "\tpop %rdx\n\tmov %rdx, %rbx\n";
    int step = 1;
    if (current == STEP) {
        current = (TOKEN) lexer->yylex();
        if (current != NUMBER) Error("Expected number after STEP");
        step = atoi(lexer->YYText());
        current = (TOKEN) lexer->yylex();
    }
    if (current != DO) Error("Expected 'DO'");
    current = (TOKEN) lexer->yylex();
    cout << "TestFor" << forTag << ":\n";
    cout << "\tmov " << var << "(%rip), %rax\n";
    cout << "\tcmp %rbx, %rax\n";
    cout << "\tjg EndFor" << forTag << "\n";
    cout << "LoopFor" << forTag << ":\n";
    StructuredStatement();
    cout << "\tmov " << var << "(%rip), %rax\n";
    cout << "\tadd $" << step << ", %rax\n";
    cout << "\tmov %rax, " << var << "(%rip)\n";
    cout << "\tjmp TestFor" << forTag << "\n";
    cout << "EndFor" << forTag << ":\n";
}
```

5. **CASE Expression OF … END**:  
   - Evaluate expression, pop into appropriate register (`%rax`, `%xmm0`, or `%bl`), for each `CaseLabelList`, emit comparisons and jumps to `CaseMatchLabel`, then code block, `jmp EndCaseTag`. Label `SkipLabel:` if no match, final label `EndCaseTag:`.  


```cpp
void CaseStatement(void) {
    cout << "\t#CASE STARTED\n";
    current = (TOKEN) lexer->yylex(); // skip CASE
    TYPE exprType = Expression();
    if (exprType != TYPE_CHAR && exprType != TYPE_UNSIGNED_INT && exprType != TYPE_DOUBLE)
        TypeError("Invalid CASE expression type");
    if (exprType == TYPE_DOUBLE) {
        cout << "\tmovsd (%rsp), %xmm0\n\taddq $8, %rsp\n";
    } else if (exprType == TYPE_CHAR) {
        cout << "\tpop %rax\n\tmov %al, %bl\n";
    } else {
        cout << "\tpop %rax\n";
    }
    CaseListElement(exprType, caseTag);
    while (current == SEMICOLON) {
        current = (TOKEN) lexer->yylex();
        if (current == END) break;
        CaseListElement(exprType, caseTag);
    }
    if (current != END) Error("Expected 'END' after CASE");
    cout << "EndCase" << caseTag << ":\n";
    current = (TOKEN) lexer->yylex(); // skip END
}
```

#### 3.5. Procedures

- **ProcedureDeclaration**  
  1. `PROCEDURE Name;`, emit `.text`, `.globl Name`, label `Name:`, prologue (`push %rbp; mov %rsp, %rbp`), generate the `CompoundStatement`, epilogue (`leave; ret`).  
  2. Procedure calls use `sub $8, %rsp; call OtherProcedure; add $8, %rsp`.  


```cpp
void ProcedureDeclaration() {
    current = (TOKEN) lexer->yylex(); // skip PROCEDURE
    string procName = lexer->YYText();
    Procedures[procName] = true;
    cout << "\n\t.text\n\t.globl " << procName << "\n" << procName << ":\n";
    current = (TOKEN) lexer->yylex(); // skip ID
    if (current != SEMICOLON) Error("Expected ';' after procedure name");
    current = (TOKEN) lexer->yylex(); // skip SEMICOLON
    cout << "\tpush %rbp\n\tmov %rsp, %rbp\n";
    CompoundStatement();
    if (current != SEMICOLON) Error("Expected ';' after procedure END");
    current = (TOKEN) lexer->yylex(); // skip SEMICOLON
    cout << "\tleave\n\tret\n";
}
```

---

## Runtime support

- **String Literals**: Collected during parsing, emitted in `.rodata` as:
  ```asm
  StrConstN:
      .string "<content>"
  ```  

- **`__concatStrings`**: A runtime helper that concatenates two `char*` pointers (`%rdi` and `%rsi`) and returns a new `char*` in `%rax`.  
- **`printf@PLT`**: Used for all `DISPLAY` calls. Format strings in `.rodata`:  
  ```asm
  FormatUInt:   .asciz "%llu\n"
  FormatString: .asciz "%s\n"
  FormatString2:.asciz "%f\n"
  FormatString3:.asciz "%c\n"
  ```

---

## Project files

- **`compilateur.cpp`**:  
  - Implements the compiler using Flex for tokenization and a recursive‐descent parser.  
  - Contains global enums and maps (`VariablesWithTypes`, `Procedures`, `TYPE`, `TOKEN`).  
  - Functions: `Expression()`, `Term()`, `Factor()`, `Identifier()`, `DisplayStatement()`, `IfStatement()`, `WhileStatement()`, `RepeatStatement()`, `ForStatement()`, `CaseStatement()`, `VarDeclaration()`, `ProcedureDeclaration()`, `FunctionDeclaration()`, `Program()`, etc.  
  - Helpers: `PromoteToDoubleIfNeeded`, `CheckBinaryOperationTypes`, `DumpStringLiterals`.  

```cpp
// Example structure of compilateur.cpp
int main(void) {
    cout << "\t\t\t# This code was produced by the CERI Compiler\n";
    current = (TOKEN) lexer->yylex();
    Program();
    cout << "\tleave\n\tret\n";
    cout << "\n\t.section .rodata\n";
    cout << "FormatUInt: .asciz \"%llu\\n\"\n";
    cout << "FormatString: .asciz \"%s\\n\"\n";
    cout << "FormatString2: .asciz \"%f\\n\"\n";
    cout << "FormatString3: .asciz \"%c\\n\"\n";
    DumpStringLiterals();
    cout << "\t.section .note.GNU-stack,\"\",@progbits\n";
    if (current != FEOF) Error("Extra characters after program");
}
```

- **`test.p`**:  
  - Sample program exercising every feature:  
    1. Global declarations of various types.  
    2. Procedure `BuildMessageExtended`: concatenates `sA`, `" | "`, `sB`, `" | "`, `sD`, then appends `sE` and `"!"`, stores in `sC` and prints; tests empty concatenation.  
    3. Procedure `DeepNested`: `WHILE b1 DO` with a `FOR i := 1 TO 3` (`IF i % 2` to handle odd/even, CASE, REPEAT, and calls `BuildMessageExtended`), then `b1 := FALSE`.  
    4. Procedure `EvenMoreComplex`: double/integer addition, `FOR k := 1 TO 2` with CASE (calls `DeepNested` on `k=1`, `WHILE` on `k=2`), then `k := 0; REPEAT FOR i := 1 TO 2 DO d1 := d1 + i; DISPLAY(d1); k := k + 1 UNTIL k > 1`.  
    5. Main block: initializes globals, arithmetic tests, `DISPLAY` of char, simple string concatenation, `BuildMessageExtended`, `IF` in CASE, `FOR i := 0 TO 3`, nested `IF`/`WHILE`, `REPEAT`/`CASE`, successive boolean/char displays, calls to `DeepNested` and `EvenMoreComplex`, a `FOR i := 5 TO 15 STEP 5` with nested `IF`, and final concatenation `resStr := resStr + " & Done"; DISPLAY(resStr)`.  

```pascal
{ Test plus poussé sans utiliser CHR, uniquement avec chaînes littérales ou variables globales }
VAR
  i, j, k, m: INTEGER;
  d1, d2, d3, d4: DOUBLE;
  b1, b2: BOOLEAN;
  c1, c2: CHAR;
  sA, sB, sC, sD, sE, resStr: STRING.

PROCEDURE BuildMessageExtended;
BEGIN
  sC := sA + " | " + sB + " | " + sD;
  sC := sC + " ~ " + sE + "!";
  resStr := sC + " - iteration X";
  DISPLAY(resStr);

  sC := "" + sB + " * ";
  DISPLAY(sC);
END;

PROCEDURE DeepNested;
BEGIN
  b1 := TRUE;
  WHILE b1 DO
  BEGIN
    DISPLAY(">> Entering DeepNested");
    FOR i := 1 TO 3 DO
    BEGIN
      IF (i % 2) == 1 THEN
      BEGIN
        DISPLAY("i is odd: Value X");
        CASE i OF
          1: DISPLAY("Case1 in DeepNested");
          3: DISPLAY("Case3 in DeepNested");
        END;
      END
      ELSE
      BEGIN
        DISPLAY("i is even: Value X");
        j := 0;
        REPEAT
          DISPLAY("  j = 0");
          j := j + 1
        UNTIL j > 1;
      END;
      BuildMessageExtended;
    END;
    b1 := FALSE;
  END;
END;

PROCEDURE EvenMoreComplex;
BEGIN
  m := 2;
  d4 := d3 + m;
  DISPLAY(d4);

  FOR k := 1 TO 2 DO
  BEGIN
    CASE k OF
      1:
      BEGIN
        DISPLAY("Case 1 in EvenMoreComplex");
        DeepNested;
      END;
      2:
      BEGIN
        DISPLAY("Case 2 in EvenMoreComplex");
        b2 := TRUE;
        i := 0;
        WHILE b2 DO
        BEGIN
          DISPLAY("   In WHILE of Case 2, i = X");
          i := i + 1;
          IF i >= 2 THEN
            b2 := FALSE;
        END;
      END;
    END;
  END;

  k := 0;
  REPEAT
    FOR i := 1 TO 2 DO
    BEGIN
      d1 := d1 + i;
      DISPLAY(d1);
    END;
    k := k + 1;
  UNTIL k > 1;
END;

BEGIN
  i := 2;
  j := 4;
  k := 0;
  m := 0;
  d1 := 3.2;
  d2 := 1.8;
  d3 := 0.0;
  d4 := 0.0;
  b1 := TRUE;
  b2 := FALSE;
  c1 := 'X';
  c2 := 'Y';
  sA := "Hello";
  sB := "World";
  sD := "Lang";
  sE := "Test";

  d3 := d1 + j;
  DISPLAY(d3);

  d3 := d3 - d2;
  DISPLAY(d3);

  DISPLAY(c1);

  resStr := sA + " " + sB;
  DISPLAY(resStr);

  BuildMessageExtended;

  i := 1;
  CASE i OF
    1:
      IF d3 > 5.0 THEN
        DISPLAY("d3 is greater than 5")
      ELSE
        DISPLAY("d3 is not greater than 5");
  END;

  FOR i := 0 TO 3 DO
  BEGIN
    IF (i % 2) == 0 THEN
      DISPLAY("i even: X")
    ELSE
      DISPLAY("i odd: X");
  END;

  IF j > 2 THEN
  BEGIN
    i := 0;
    WHILE i < 2 DO
    BEGIN
      DISPLAY("WHILE in main, i = X");
      i := i + 1;
    END;
  END
  ELSE
    DISPLAY("Skipping WHILE because j <= 2");

  d1 := 1.0;
  k := 0;
  REPEAT
    CASE k OF
      0: DISPLAY("Repeat k=0");
      1: DISPLAY("Repeat k=1");
    END;
    d1 := d1 + 0.5;
    DISPLAY(d1);
    k := k + 1
  UNTIL k > 2;

  DISPLAY(b1);
  b1 := FALSE;
  DISPLAY(b1);
  b1 := 0;
  DISPLAY(b1);
  b1 := 1;
  DISPLAY(b1);

  DISPLAY(c2);
  c2 := 'C';
  DISPLAY(c2);

  DeepNested;

  EvenMoreComplex;

  FOR i := 5 TO 15 STEP 5 DO
  BEGIN
    m := i * 2;
    IF m > 20 THEN
      DISPLAY("m is large: " + sA + " " + sB)
    ELSE
      DISPLAY("m = X");
  END;

  resStr := resStr + " & Done";
  DISPLAY(resStr);
END.
```

---

## Detailed Feature List

1. **Numeric Conversions**  
   - **Integer → Double**: When an integer appears in a double expression, we pop the integer, use `cvtsi2sd`, then reserve 8 bytes and store the double.  
   - **Promotion in Mixed Expressions**: Call `PromoteToDoubleIfNeeded(type2,1)` and `PromoteToDoubleIfNeeded(type1,2)` to ensure two true doubles for x87.  
   - **Double FPU arithmetic**: Use x87 (`fldl`, `faddp`, `fsubp`, `fmulp`, `fdivp`, `fstpl`) and store back to memory.  


```cpp
TYPE PromoteToDoubleIfNeeded(TYPE type, int position) {
    if (type == TYPE_UNSIGNED_INT) {
        if (position == 1) {
            cout << "\tpop %rax\n\tcvtsi2sd %rax, %xmm0\n";
            cout << "\tsubq $8, %rsp\n\tmovsd %xmm0, (%rsp)\n";
        } else {
            cout << "\tfldl  (%rsp)\n\taddq  $8, %rsp\n";
            cout << "\tpop   %rbx\n\tcvtsi2sd %rbx, %xmm1\n";
            cout << "\tsubq  $8, %rsp\n\tmovsd %xmm1, (%rsp)\n";
            cout << "\tsubq  $8, %rsp\n\tfstpl (%rsp)\n";
        }
        return TYPE_DOUBLE;
    } else if (type == TYPE_DOUBLE) {
        if (position == 1) {
            cout << "\tfldl  (%rsp)\n\taddq $8, %rsp\n\tfstpl (%rsp)\n";
        } else {
            cout << "\tfldl  (%rsp)\n\taddq $8, %rsp\n";
            cout << "\tfldl  (%rsp)\n\taddq $8, %rsp\n";
            cout << "\tsubq  $8, %rsp\n\tfstpl (%rsp)\n";
            cout << "\tsubq  $8, %rsp\n\tfstpl (%rsp)\n";
        }
        return TYPE_DOUBLE;
    }
    return type;
}
```

2. **Boolean Representation & Conversion**  
   - Stored as one byte (`.byte 0` or `.byte 0xFF`).  
   - Loaded with `movzbl var(%rip), %eax; push %rax`.  
   - Printed by popping into `%rax`, `testb %al, %al; setne %al; movzbq %al, %rax`, then `printf("%llu\n", rax)`.  


```cpp
// Printing a boolean:
cout << "\tpop %rax\n\ttestb %al, %al\n\tsetne %al\n\tmovzbq %al, %rax\n";
cout << "\tmov %rax, %rsi\n\tleaq FormatUInt(%rip), %rdi\n\tmovl $0, %eax\n";
cout << "\tcall printf@PLT\n";
```

3. **Character Handling**  
   - Stored as one byte (`.byte 0`).  
   - Loaded with `movzbl c(%rip), %eax; push %rax`.  
   - Printed by popping into `%rax`, moving `%al` to `%sil`, then `printf("%c\n", sil)`.  


```cpp
// Printing a char:
cout << "\tpop %rax\n\tmovb %al, %sil\n";
cout << "\tleaq FormatString3(%rip), %rdi\n\tmovl $0, %eax\n";
cout << "\tcall printf@PLT\n";
```

4. **Strings & Concatenation**  
   - **Variables**: Each `STRING` is `.quad 0`.  
   - **String Literals**: Assigned labels `StrConstN`, emitted in `.rodata`.  
   - **Concatenation**: Pop into `%rsi` and `%rdi`, `sub $8, %rsp; call __concatStrings; add $8, %rsp; mov %rax, tempStringSlot(%rip); mov tempStringSlot(%rip), %rax; push %rax`.  


```cpp
// String concatenation:
cout << "\tpop %rsi    # first string\n\tpop %rdi    # second string\n";
cout << "\tsub $8, %rsp    # align stack before calling __concatStrings\n";
cout << "\tcall __concatStrings\n\tadd $8, %rsp\n";
cout << "\tmov %rax, tempStringSlot(%rip)\n";
cout << "\tmov tempStringSlot(%rip), %rax\n";
cout << "\tpush %rax    # Push result\n";
```

5. **Procedure Calls & Stack Alignment**  
   - Ensure `%rsp % 16 == 0` before each call:  
     ```asm
     sub   $8, %rsp  
     call  Name  
     add   $8, %rsp  
     ```  
   - Each procedure ends with `leave; ret`.  


```cpp
void CallProcedure(void) {
    string name = lexer->YYText();
    cout << "\tsub $8, %rsp    # align stack before calling " << name << "\n";
    cout << "\tcall " << name << "\n";
    cout << "\tadd $8, %rsp    # restore stack after call\n";
    current = (TOKEN) lexer->yylex();
    if (current != SEMICOLON) Error("Expected ';' after procedure call");
}
```

---

## How to Build & Run

1. **Compile the compiler**:  
   ```bash
   flex tokeniser.l
   g++ -std=c++17 -o compilateur lex.yy.cc compilateur.cpp -lfl
   ```  
   This produces `compilateur`.  

2. **Example**:  
   ```bash
   ./compilateur < test.p > test.s
   gcc -no-pie -o test test.s concat_strings.o
   ./test
   ```  

---

## Testing & Sample Outputs

Using `test.p`, the compiler emits `test.s`. After linking:

![alt text](image-1.png)

---

## Future Enhancements

- **Local variables**  
- **Function return values**  
- **Better string memory management**  
- **Error recovery**  
- **Optimizations**  

---

## Acknowledgments

- Flex/Bison tokenization  
- x86-64 System V ABI documentation  
- Peter Jourlin’s CERI Compiler skeleton  


