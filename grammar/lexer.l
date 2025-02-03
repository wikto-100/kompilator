%{
/* C++ string header, for string ops below */
#include <string>

/* Implementation of yyFlexScanner */ 
#include "scanner.hpp"
#undef  YY_DECL
#define YY_DECL int Compiler::Scanner::yylex( Compiler::Parser::semantic_type * const lval, Compiler::Parser::location_type *loc )

/* typedef to make the returns for the tokens shorter */
using token = Compiler::Parser::token;


/* msvc2010 requires that we exclude this header file. */
#define YY_NO_UNISTD_H

/* update location on matching */
#define YY_USER_ACTION loc->step(); loc->columns(yyleng);

%}

%option debug
%option nodefault
%option yyclass="Compiler::Scanner"
%option noyywrap
%option c++

%%
%{          /** Code executed at the beginning of yylex **/
            yylval = lval;
%}

[ \t\r]+                     ;
[\n]        { loc->lines(); }
#[^\n]*                      ;

"PROGRAM"                    { return token::PROGRAM; }
"PROCEDURE"                  { return token::PROCEDURE; }
"IF"                         { return token::IF; }
"THEN"                       { return token::THEN; }
"ELSE"                       { return token::ELSE; }
"ENDIF"                      { return token::ENDIF; }
"WHILE"                      { return token::WHILE; }
"DO"                         { return token::DO; }
"ENDWHILE"                   { return token::ENDWHILE; }
"REPEAT"                     { return token::REPEAT; }
"UNTIL"                      { return token::UNTIL; }
"FOR"                        { return token::FOR; }
"FROM"                       { return token::FROM; }
"TO"                         { return token::TO; }
"DOWNTO"                     { return token::DOWNTO; }
"ENDFOR"                     { return token::ENDFOR; }
"READ"                       { return token::READ; }
"WRITE"                      { return token::WRITE; }
"BEGIN"                      { return token::KW_BEGIN; }
"END"                        { return token::KW_END; }
"IS"                         { return token::IS; }
"T"                          { return token::T; }

":="                         { return token::ASSIGN; }
"!="                         { return token::NEQ; }
">="                         { return token::GEQ; }
"<="                         { return token::LEQ; }
"["                          { return token::LBRACKET; }
"]"                          { return token::RBRACKET; }
"("                          { return token::LPAREN; }
")"                          { return token::RPAREN; }
","                          { return token::COMMA; }
":"                          { return token::COLON; }
";"                          { return token::SEMI; }
"%"                          { return token::PERCENT; }
"="                          { return token::EQ; }
">"                          { return token::GT; }
"<"                          { return token::LT; }
"+"                          { return token::PLUS; }
"-"                          { return token::MINUS; }
"*"                          { return token::STAR; }
"/"                          { return token::SLASH; }
[_a-z]+     {
               yylval->build< std::string >( yytext );
               return( token::pidentifier );
            }
[0-9]+      {    
               yylval->build< std::string >( yytext );
               return token::num;
            }


.           {
                  std::cerr << "Unrecognized character \""
                                          << yytext << "\" at line "
                                          << yylineno << std::endl;
                                exit(1);
            }
%%


