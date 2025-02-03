%skeleton "lalr1.cc"
%require  "3.8"
%debug
%defines
%define api.namespace {Compiler}

/** Autor: Wiktor Stojek, nr. indeksu 272383 */


%define api.parser.class {Parser}

/** std::variant dla różnych typów */
%define api.value.type variant

%code requires {
   #include <memory> // std::shared_ptr
   #include <string>
   #include <vector>
   #include <optional>

   // Definicje z AST
   #include "ast.hpp"

   using CA = ::Compiler::AST;

   namespace Compiler {
      class Driver;
      class Scanner;
   }

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif
}

%parse-param { Compiler::Scanner  &scanner  }
%parse-param { Compiler::Driver   &driver   }

%code top {
   #include <iostream>
   #include <cstdlib>
   #include <fstream>
}

%code {
   #include "driver.hpp"
   #undef yylex
   #define yylex scanner.yylex

   static long long toLongLong(const std::string &s) {
      return std::stoll(s);
   }
}

/* =====================================================================
   TOKENY
   ===================================================================== */
%token PROGRAM PROCEDURE IS
%token KW_BEGIN
%token KW_END
%token IF THEN ELSE ENDIF
%token WHILE DO ENDWHILE
%token REPEAT UNTIL
%token FOR FROM TO DOWNTO ENDFOR
%token READ WRITE
%token T

%token <std::string> pidentifier
%token <std::string> num

%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token COMMA
%token COLON
%token SEMI
%token ASSIGN
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token EQ
%token NEQ
%token GT
%token LT
%token GEQ
%token LEQ

/* =====================================================================
   NIETERMINALE
   ===================================================================== */
%type <std::shared_ptr<CA::ProgramAll>> program_all
%type <std::vector<std::shared_ptr<CA::Procedure>>> procedures
%type <std::shared_ptr<CA::Procedure>> proc_head
%type <std::shared_ptr<CA::Main>> main
%type <std::vector<std::shared_ptr<CA::Command>>> commands
%type <std::shared_ptr<CA::Command>> command
%type <std::shared_ptr<CA::Condition>> condition
%type <std::shared_ptr<CA::Expression>> expression
%type <std::shared_ptr<CA::Value>> value
%type <CA::Identifier> identifier

%type <std::shared_ptr<CA::Declarations>> declarations
%type <std::vector<std::pair<bool,std::string>>> args_decl
%type <std::vector<std::pair<bool,std::string>>> args
%type <std::shared_ptr<CA::ProcCallCmd>> proc_call

%start program_all

%locations

%%
/* =====================================================================
   GRAMATYKA
   ===================================================================== */

/* program_all -> procedures main */
program_all:
    procedures main
    {
      auto root = std::make_shared<CA::ProgramAll>();
      // $1 is a std::vector<std::shared_ptr<CA::Procedure>>
      for (auto &p : $1) {
         root->procedures.push_back(p);
      }
      // $2 is std::shared_ptr<CA::Main>
      root->mainPart = $2;
      // $$ is std::shared_ptr<CA::ProgramAll>
      $$ = root;
      driver.setASTRoot(root);
    }
  ;

/* procedures -> 
       procedures PROCEDURE proc_head IS declarations KW_BEGIN commands KW_END
     | procedures PROCEDURE proc_head IS KW_BEGIN commands KW_END
     | (empty)
*/
procedures:
      procedures PROCEDURE proc_head IS declarations KW_BEGIN commands KW_END
    {
      // $1 => std::vector<std::shared_ptr<CA::Procedure>>
      // $3 => std::shared_ptr<CA::Procedure>
      // $5 => std::shared_ptr<CA::Declarations>
      // $7 => std::vector<std::shared_ptr<CA::Command>>
      auto vec   = $1;
      auto proc  = $3;
      auto decls = $5;
      auto cmds  = $7;

      if (decls) {
         proc->localDeclarations.push_back(decls);
      }
      for (auto &c : cmds) {
         proc->commands.push_back(c);
      }
      vec.push_back(proc);
      $$ = std::move(vec); 
    }
    | procedures PROCEDURE proc_head IS KW_BEGIN commands KW_END
    {
      auto vec  = $1; // std::vector<std::shared_ptr<CA::Procedure>>
      auto proc = $3; // std::shared_ptr<CA::Procedure>
      auto cmds = $6; // std::vector<std::shared_ptr<CA::Command>>

      for (auto &c : cmds) {
         proc->commands.push_back(c);
      }
      vec.push_back(proc);
      $$ = std::move(vec);
    }
    |
    {
      // Empty => create a new vector
      $$ = std::vector<std::shared_ptr<CA::Procedure>>();
    }
  ;

/* proc_head -> pidentifier ( args_decl ) */
proc_head:
    pidentifier LPAREN args_decl RPAREN
    {
      // $1 => std::string
      // $3 => std::vector<std::pair<bool,std::string>>
      auto proc = std::make_shared<CA::Procedure>();
      proc->name = $1;
      for (auto &argPair : $3) {
         proc->arguments.push_back(argPair);
      }
      
      $$ = proc; // std::shared_ptr<CA::Procedure>
    }
  ;

/*
 main -> PROGRAM IS declarations KW_BEGIN commands KW_END
       | PROGRAM IS KW_BEGIN commands KW_END
*/
main:
      PROGRAM IS declarations KW_BEGIN commands KW_END
    {
      auto m = std::make_shared<CA::Main>();
      // $3 => std::shared_ptr<CA::Declarations>
      if ($3) {
         m->declarations.push_back($3);
      }
      // $5 => std::vector<std::shared_ptr<CA::Command>>
      for (auto &cmd : $5) {
         m->commands.push_back(cmd);
      }
      $$ = m;
    }
    | PROGRAM IS KW_BEGIN commands KW_END
    {
      auto m = std::make_shared<CA::Main>();
      // $4 => std::vector<std::shared_ptr<CA::Command>>
      for (auto &cmd : $4) {
         m->commands.push_back(cmd);
      }
      $$ = m;
    }
  ;

/* commands -> commands command | command */
commands:
    commands command
    {
      auto vec = $1; // std::vector<std::shared_ptr<CA::Command>>
      vec.push_back($2); 
      $$ = std::move(vec);
    }
    | command
    {
      std::vector<std::shared_ptr<CA::Command>> vec;
      vec.push_back($1);
      $$ = std::move(vec);
    }
  ;

/*
 command ->
    identifier ASSIGN expression ;
  | IF condition THEN commands ELSE commands ENDIF
  | IF condition THEN commands ENDIF
  | WHILE condition DO commands ENDWHILE
  | REPEAT commands UNTIL condition ;
  | FOR pidentifier FROM value TO value DO commands ENDFOR
  | FOR pidentifier FROM value DOWNTO value DO commands ENDFOR
  | proc_call ;
  | READ identifier ;
  | WRITE value ;
*/
command:
    identifier ASSIGN expression SEMI
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::ASSIGN;
      auto assignNode = std::make_shared<CA::AssignCmd>();
      assignNode->lhs = $1;   // $1 => CA::Identifier
      assignNode->rhs = $3;   // $3 => std::shared_ptr<CA::Expression>
      cmd->assignCmd = assignNode;
      $$ = cmd;
    }
    | IF condition THEN commands ELSE commands ENDIF
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::IF_THEN_ELSE;
      auto ifNode = std::make_shared<CA::IfCmd>();
      ifNode->condition = $2; 
      for (auto &c : $4) {
         ifNode->thenCommands.push_back(c);
      }
      for (auto &c : $6) {
         ifNode->elseCommands.push_back(c);
      }
      cmd->ifCmd = ifNode;
      $$ = cmd;
    }
    | IF condition THEN commands ENDIF
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::IF_THEN;
      auto ifNode = std::make_shared<CA::IfCmd>();
      ifNode->condition = $2;
      for (auto &c : $4) {
         ifNode->thenCommands.push_back(c);
      }
      cmd->ifCmd = ifNode;
      $$ = cmd;
    }
    | WHILE condition DO commands ENDWHILE
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::WHILE;
      auto w = std::make_shared<CA::WhileCmd>();
      w->condition = $2;
      for (auto &c : $4) {
         w->body.push_back(c);
      }
      cmd->whileCmd = w;
      $$ = cmd;
    }
    | REPEAT commands UNTIL condition SEMI
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::REPEAT;
      auto r = std::make_shared<CA::RepeatCmd>();
      for (auto &c : $2) {
         r->body.push_back(c);
      }
      r->condition = $4;
      cmd->repeatCmd = r;
      $$ = cmd;
    }
    | FOR pidentifier FROM value TO value DO commands ENDFOR
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::FOR_TO;
      auto f = std::make_shared<CA::ForCmd>();
      f->loopVar   = $2;
      f->downTo    = false;
      f->startValue= $4;
      f->endValue  = $6;
      for (auto &c : $8) {
         f->body.push_back(c);
      }
      cmd->forCmd = f;
      $$ = cmd;
    }
    | FOR pidentifier FROM value DOWNTO value DO commands ENDFOR
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::FOR_DOWNTO;
      auto f = std::make_shared<CA::ForCmd>();
      f->loopVar    = $2;
      f->downTo     = true;
      f->startValue = $4;
      f->endValue   = $6;
      for (auto &c : $8) {
         f->body.push_back(c);
      }
      cmd->forCmd = f;
      $$ = cmd;
    }
    | proc_call SEMI
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::PROC_CALL;
      cmd->procCallCmd = $1;
      $$ = cmd;
    }
    | READ identifier SEMI
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::READ;
      auto io = std::make_shared<CA::IOCommand>();
      io->ioType = CA::IOType::READ;
      io->readTarget = $2;
      cmd->ioCmd = io;
      $$ = cmd;
    }
    | WRITE value SEMI
    {
      auto cmd = std::make_shared<CA::Command>();
      cmd->type = CA::CommandType::WRITE;
      auto io = std::make_shared<CA::IOCommand>();
      io->ioType = CA::IOType::WRITE;
      io->writeValue = $2;
      cmd->ioCmd = io;
      $$ = cmd;
    }
  ;

/* proc_call -> pidentifier ( args ) */
proc_call:
    pidentifier LPAREN args RPAREN
    {
      auto call = std::make_shared<CA::ProcCallCmd>();
      call->name = $1; // $1 => std::string
      for (auto &argPair : $3) {
         call->args.push_back(argPair.second);
      }
      $$ = call; 
    }
  ;

/*
 declarations ->
   declarations COMMA pidentifier
 | declarations COMMA pidentifier [ num : num ]
 | pidentifier
 | pidentifier [ num : num ]
*/
declarations:
    declarations COMMA pidentifier
    {
      auto ds = $1; // std::shared_ptr<CA::Declarations>
      CA::DeclItem it;
      it.name = $3;
      it.isArray = false;
      ds->items.push_back(it);
      $$ = ds;
    }
    | declarations COMMA pidentifier LBRACKET num COLON num RBRACKET
    {
      auto ds = $1;
      CA::DeclItem it;
      it.name = $3;
      it.isArray = true;
      it.rangeStart = toLongLong($5);
      it.rangeEnd   = toLongLong($7);
      ds->items.push_back(it);
      $$ = ds;
    }
    | declarations COMMA pidentifier LBRACKET MINUS num COLON num RBRACKET
    {
      auto ds = $1;
      CA::DeclItem it;
      it.name = $3;
      it.isArray = true;
      it.rangeStart = -toLongLong($6);
      it.rangeEnd   = toLongLong($8);
      ds->items.push_back(it);
      $$ = ds;
    }
    | declarations COMMA pidentifier LBRACKET num COLON MINUS num RBRACKET
    {
      // To zawsze spowoduje błąd semantyczny (w teorii)

      auto ds = $1;
      CA::DeclItem it;
      it.name = $3;
      it.isArray = true;
      it.rangeStart = toLongLong($5);
      it.rangeEnd   = -toLongLong($8);
      ds->items.push_back(it);
      $$ = ds;
    }
    | declarations COMMA pidentifier LBRACKET MINUS num COLON MINUS num RBRACKET
    {
      // To zawsze spowoduje błąd semantyczny (w teorii)
      auto ds = $1;
      CA::DeclItem it;
      it.name = $3;
      it.isArray = true;
      it.rangeStart = -toLongLong($6);
      it.rangeEnd   = -toLongLong($9);
      ds->items.push_back(it);
      $$ = ds;
    }
    | pidentifier
    {
      auto ds = std::make_shared<CA::Declarations>();
      CA::DeclItem it;
      it.name = $1;
      it.isArray = false;
      ds->items.push_back(it);
      $$ = ds;
    }
    | pidentifier LBRACKET num COLON num RBRACKET
    {
      auto ds = std::make_shared<CA::Declarations>();
      CA::DeclItem it;
      it.name = $1;
      it.isArray = true;
      it.rangeStart = toLongLong($3);
      it.rangeEnd   = toLongLong($5);
      ds->items.push_back(it);
      $$ = ds;
    }
    | pidentifier LBRACKET MINUS num COLON num RBRACKET
    {
      auto ds = std::make_shared<CA::Declarations>();
      CA::DeclItem it;
      it.name = $1;
      it.isArray = true;
      it.rangeStart = -toLongLong($4);
      it.rangeEnd   = toLongLong($6);
      ds->items.push_back(it);
      $$ = ds;
    }
    | pidentifier LBRACKET num COLON MINUS num RBRACKET
    {
      // To zawsze spowoduje błąd semantyczny (w teorii)
      auto ds = std::make_shared<CA::Declarations>();
      CA::DeclItem it;
      it.name = $1;
      it.isArray = true;
      it.rangeStart = toLongLong($3);
      it.rangeEnd   = -toLongLong($6);
      ds->items.push_back(it);
      $$ = ds;
    }
    | pidentifier LBRACKET MINUS num COLON MINUS num RBRACKET
    {
      auto ds = std::make_shared<CA::Declarations>();
      CA::DeclItem it;
      it.name = $1;
      it.isArray = true;
      it.rangeStart = -toLongLong($4);
      it.rangeEnd   = -toLongLong($7);
      ds->items.push_back(it);
      $$ = ds;
    }
  ;

/*
 args_decl ->
    args_decl COMMA pidentifier
  | args_decl COMMA T pidentifier
  | pidentifier
  | T pidentifier
*/
args_decl:
    args_decl COMMA pidentifier
    {
      auto vec = $1;
      vec.push_back({false, $3});
      $$ = std::move(vec);
    }
    | args_decl COMMA T pidentifier
    {
      auto vec = $1;
      vec.push_back({true, $4});
      $$ = std::move(vec);
    }
    | pidentifier
    {
      std::vector<std::pair<bool,std::string>> vec;
      vec.push_back({false, $1});
      $$ = std::move(vec);
    }
    | T pidentifier
    {
      std::vector<std::pair<bool,std::string>> vec;
      vec.push_back({true, $2});
      $$ = std::move(vec);
    }
  ;

/*
 args ->
    args COMMA pidentifier
  | pidentifier
*/
args:
    args COMMA pidentifier
    {
      auto vec = $1;
      vec.push_back({false, $3});
      $$ = std::move(vec);
    }
    | pidentifier
    {
      std::vector<std::pair<bool,std::string>> vec;
      vec.push_back({false, $1});
      $$ = std::move(vec);
    }
  ;

/*
 expression ->
   value
 | value PLUS value
 | value MINUS value
 | value STAR value
 | value SLASH value
 | value PERCENT value
*/
expression:
    value
    {
      auto expr = std::make_shared<CA::Expression>();
      expr->op = CA::ExprOp::NONE;
      expr->left = $1;
      $$ = expr;
    }
    | value PLUS value
    {
      auto expr = std::make_shared<CA::Expression>();
      expr->op = CA::ExprOp::PLUS;
      expr->left = $1;
      expr->right= $3;
      $$ = expr;
    }
    | value MINUS value
    {
      auto expr = std::make_shared<CA::Expression>();
      expr->op = CA::ExprOp::MINUS;
      expr->left = $1;
      expr->right= $3;
      $$ = expr;
    }
    | value STAR value
    {
      auto expr = std::make_shared<CA::Expression>();
      expr->op = CA::ExprOp::STAR;
      expr->left = $1;
      expr->right= $3;
      $$ = expr;
    }
    | value SLASH value
    {
      auto expr = std::make_shared<CA::Expression>();
      expr->op = CA::ExprOp::SLASH;
      expr->left = $1;
      expr->right= $3;
      $$ = expr;
    }
    | value PERCENT value
    {
      auto expr = std::make_shared<CA::Expression>();
      expr->op = CA::ExprOp::PERCENT;
      expr->left = $1;
      expr->right= $3;
      $$ = expr;
    }
  ;

/*
 condition ->
    value EQ value
  | value NEQ value
  | value GT value
  | value LT value
  | value GEQ value
  | value LEQ value
*/
condition:
    value EQ value
    {
      auto cond = std::make_shared<CA::Condition>();
      cond->op = CA::CondOp::EQ;
      cond->left  = $1;
      cond->right = $3;
      $$ = cond;
    }
    | value NEQ value
    {
      auto cond = std::make_shared<CA::Condition>();
      cond->op = CA::CondOp::NEQ;
      cond->left  = $1;
      cond->right = $3;
      $$ = cond;
    }
    | value GT value
    {
      auto cond = std::make_shared<CA::Condition>();
      cond->op = CA::CondOp::GT;
      cond->left  = $1;
      cond->right = $3;
      $$ = cond;
    }
    | value LT value
    {
      auto cond = std::make_shared<CA::Condition>();
      cond->op = CA::CondOp::LT;
      cond->left  = $1;
      cond->right = $3;
      $$ = cond;
    }
    | value GEQ value
    {
      auto cond = std::make_shared<CA::Condition>();
      cond->op = CA::CondOp::GEQ;
      cond->left  = $1;
      cond->right = $3;
      $$ = cond;
    }
    | value LEQ value
    {
      auto cond = std::make_shared<CA::Condition>();
      cond->op = CA::CondOp::LEQ;
      cond->left  = $1;
      cond->right = $3;
      $$ = cond;
    }
  ;

/*
 value ->
   num
 | identifier
*/
value:
    num
    {
      auto val = std::make_shared<CA::Value>();
      val->isIdentifier = false;
      val->numberValue = toLongLong($1);
      $$ = val;
    }
    | MINUS num
    {
      auto val = std::make_shared<CA::Value>();
      val->isIdentifier = false;
      val->numberValue = -toLongLong($2);
      $$ = val;
    }
    | identifier
    {
      auto val = std::make_shared<CA::Value>();
      val->isIdentifier = true;
      val->identifier = $1;
      $$ = val;
    }
  ;

/*
 identifier ->
    pidentifier
  | pidentifier LBRACKET pidentifier RBRACKET
  | pidentifier LBRACKET num RBRACKET
*/
identifier:
    pidentifier
    {
      CA::Identifier id;
      id.name        = $1;
      id.idxType     = CA::IdentifierIndexType::NONE;
      id.indexNumber = 0;
      id.line        = @1.begin.line;
      id.column      = @1.begin.column;
      $$ = id;
    }
    | pidentifier LBRACKET pidentifier RBRACKET
    {
      CA::Identifier id;
      id.name     = $1;
      id.idxType  = CA::IdentifierIndexType::VARIABLE;
      id.indexVar = $3;
      id.line     = @1.begin.line;
      id.column   = @1.begin.column;
      $$ = id;
    }
    | pidentifier LBRACKET num RBRACKET
    {
      CA::Identifier id;
      id.name       = $1;
      id.idxType    = CA::IdentifierIndexType::NUMERIC;
      id.indexNumber= toLongLong($3);
      id.line       = @1.begin.line;
      id.column     = @1.begin.column;
      $$ = id;
    }
    | pidentifier LBRACKET MINUS num RBRACKET
    {
      CA::Identifier id;
      id.name       = $1;
      id.idxType    = CA::IdentifierIndexType::NUMERIC;
      id.indexNumber= -toLongLong($4);
      id.line       = @1.begin.line;
      id.column     = @1.begin.column;
      $$ = id;
    }
  ;

%%



void 
Compiler::Parser::error(const location_type &l, const std::string &err_message)
{
   std::cerr << "Błąd (Bison): " << err_message << " linia.kolumna: " << l << "\n";
}
