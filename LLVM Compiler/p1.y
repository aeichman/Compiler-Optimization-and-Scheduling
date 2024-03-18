%{
#include <cstdio>
#include <list>
#include <vector>
#include <map>
#include <iostream>
#include <string>
#include <memory>
#include <stdexcept>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/FileSystem.h"

using namespace llvm;
using namespace std;

// Need for parser and scanner
extern FILE *yyin;
int yylex();
void yyerror(const char*);
int yyparse();

// Needed for LLVM
string funName;
Module *M;
LLVMContext TheContext;
IRBuilder<> Builder(TheContext);
map<string, Value*> idValuePair;
%}

%union {
  vector<string> *params_list;
  Value *val;
  int num;
  string *id;
}

/*%define parse.trace*/

%type <params_list> params_list
%type <val> expr
%type <val> ensemble

%token IN FINAL
%token ERROR
%token <num> NUMBER
%token <id> ID
%token BINV INV PLUS MINUS XOR AND OR MUL DIV MOD
%token COMMA ENDLINE ASSIGN LBRACKET RBRACKET LPAREN RPAREN NONE COLON
%token REDUCE EXPAND

%precedence BINV
%precedence INV
%left PLUS MINUS OR
%left MUL DIV AND XOR MOD

%start program

%%

program: inputs statements_opt final
{
  YYACCEPT;
}
;

inputs:   IN params_list ENDLINE
{
  std::vector<Type*> param_types;
  for(auto s: *$2)
    {
      param_types.push_back(Builder.getInt32Ty());
    }
  ArrayRef<Type*> Params (param_types);

  // Create int function type with no arguments
  FunctionType *FunType =
    FunctionType::get(Builder.getInt32Ty(),Params,false);

  // Create a main function
  Function *Function = Function::Create(FunType,GlobalValue::ExternalLinkage,funName,M);

  int arg_no=0;
  for(auto &a: Function->args()) {
    // iterate over arguments of function
    // match name to position

    // FIX ME @129 @105
    idValuePair[$2->at(arg_no)] = &a;
    arg_no++;

  }

  //Add a basic block to main to hold instructions, and set Builder
  //to insert there
  Builder.SetInsertPoint(BasicBlock::Create(TheContext, "entry", Function));

}
| IN NONE ENDLINE
{
  // Create int function type with no arguments
  FunctionType *FunType =
    FunctionType::get(Builder.getInt32Ty(),false);

  // Create a main function
  Function *Function = Function::Create(FunType,
         GlobalValue::ExternalLinkage,funName,M);

  //Add a basic block to main to hold instructions, and set Builder
  //to insert there
  Builder.SetInsertPoint(BasicBlock::Create(TheContext, "entry", Function));
}
;

params_list: ID
{
  $$ = new vector<string>;
  $$->push_back(*$1);
}
| params_list COMMA ID
{
  $$->push_back(*$3);
}
;

final: FINAL ensemble endline_opt
{
  Builder.CreateRet($2);
}
;

endline_opt: %empty | ENDLINE;


statements_opt: %empty
            | statements;

statements:   statement
            | statements statement
;

statement: ID ASSIGN ensemble ENDLINE {
    idValuePair[*$1] = $3;
}
| ID NUMBER ASSIGN ensemble ENDLINE {
    idValuePair[*$1] = Builder.getInt32($2);
}
| ID LBRACKET ensemble RBRACKET ASSIGN ensemble ENDLINE
;

ensemble:  expr {
    $$ = $1;
}
| expr COLON NUMBER                  // 566 only
| ensemble COMMA expr {
    $$ = Builder.CreateAdd(Builder.CreateShl($1, Builder.getInt32(1)), $3);
}
| ensemble COMMA expr COLON NUMBER   // 566 only
;

expr:   ID {
    $$ = idValuePair[*$1];
    //  $$ = Builder.CreateAnd(idValuePair[*$1], Builder.getInt32(1)); // FIX ME

}
| ID NUMBER {
    // idValuePair[*$1] = Builder.getInt32($2);
    // $$ = idValuePair[*$1];
    $$ = Builder.CreateAnd(Builder.CreateLShr(idValuePair[*$1], Builder.getInt32($2)), Builder.getInt32(1));

}
| NUMBER {
    $$ = Builder.getInt32($1);
}
| expr PLUS expr {
    $$ = Builder.CreateAdd($1, $3);
}
| expr MINUS expr {
    $$ = Builder.CreateSub($1, $3);
}
| expr XOR expr {
    $$ = Builder.CreateXor($1, $3);
}
| expr AND expr {
    $$ = Builder.CreateAnd($1, $3);
}
| expr OR expr {
    $$ = Builder.CreateOr($1, $3);
}
| INV expr {
    $$ = Builder.CreateNot($2);
}
| BINV expr {
    Value *getLSB = Builder.CreateAnd($2, Builder.getInt32(1));
    $$ = Builder.CreateOr(Builder.CreateAnd($2, Builder.getInt32(~1)), Builder.CreateXor(getLSB, Builder.getInt32(1)));
}
| expr MUL expr {
    $$ = Builder.CreateMul($1, $3);
}
| expr DIV expr {
    $$ = Builder.CreateUDiv($1, $3);
}
| expr MOD expr {
    $$ = Builder.CreateSRem($1, $3);
}
| ID LBRACKET ensemble RBRACKET {
    $$ = Builder.CreateAnd(Builder.CreateLShr(idValuePair[*$1], Builder.CreateAnd($3, Builder.getInt32(31))), Builder.getInt32(1));
}
| LPAREN ensemble RPAREN {
    $$ = $2;
}
/* 566 only */
| LPAREN ensemble RPAREN LBRACKET ensemble RBRACKET
| REDUCE AND LPAREN ensemble RPAREN
| REDUCE OR LPAREN ensemble RPAREN
| REDUCE XOR LPAREN ensemble RPAREN
| REDUCE PLUS LPAREN ensemble RPAREN
| EXPAND  LPAREN ensemble RPAREN
;

%%

unique_ptr<Module> parseP1File(const string &InputFilename)
{
  funName = InputFilename;
  if (funName.find_last_of('/') != string::npos)
    funName = funName.substr(funName.find_last_of('/')+1);
  if (funName.find_last_of('.') != string::npos)
    funName.resize(funName.find_last_of('.'));

  //errs() << "Function will be called " << funName << ".\n";

  // unique_ptr will clean up after us, call destructor, etc.
  unique_ptr<Module> Mptr(new Module(funName.c_str(), TheContext));

  // set global module
  M = Mptr.get();

  /* this is the name of the file to generate, you can also use
     this string to figure out the name of the generated function */
  yyin = fopen(InputFilename.c_str(),"r");

  //yydebug = 1;
  if (yyparse() != 0)
    // errors, so discard module
    Mptr.reset();
  else
    // Dump LLVM IR to the screen for debugging
    M->print(errs(),nullptr,false,true);

  return Mptr;
}

void yyerror(const char* msg)
{
  printf("%s\n",msg);
}