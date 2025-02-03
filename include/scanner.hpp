#ifndef __SCANNER_HPP__
#define __SCANNER_HPP__ 1

#if !defined(yyFlexLexerOnce)
#include <FlexLexer.h>
#endif

#include "parser.tab.hh"
#include "location.hh"

namespace Compiler
{

   class Scanner : public yyFlexLexer
   {
   public:
      Scanner(std::istream *in) : yyFlexLexer(in) {
                                     };
      virtual ~Scanner() {

      };

      // get rid of override virtual function warning
      using FlexLexer::yylex;

      virtual int yylex(Compiler::Parser::semantic_type *const lval,
                        Compiler::Parser::location_type *location);
      // YY_DECL defined in lexer.l
      // Method body created by flex in lexer.yy.cc

   private:
      /* yyval ptr */
      Compiler::Parser::semantic_type *yylval = nullptr;
   };

} /* end namespace Compiler */

#endif /* END __SCANNER_HPP__ */
