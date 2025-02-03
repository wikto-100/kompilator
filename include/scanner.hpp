#ifndef __SCANNER_HPP__
#define __SCANNER_HPP__ 1
/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#if !defined(yyFlexLexerOnce)
#include <FlexLexer.h>
#endif

#include "parser.tab.hh"
#include "location.hh"

namespace Compiler
{
   /**
    * \brief Klasa skanera dla języka maszyny wirtualnej. (Integracja C++/Flex/Bison)
    */
   class Scanner : public yyFlexLexer
   {
   public:
      Scanner(std::istream *in) : yyFlexLexer(in) {};
      virtual ~Scanner() {};

      using FlexLexer::yylex;

      
      virtual int yylex(Compiler::Parser::semantic_type *const lval,
                        Compiler::Parser::location_type *location);
   private:
      Compiler::Parser::semantic_type *yylval = nullptr;
   };

} // namespace Compiler

#endif /* END __SCANNER_HPP__ */
