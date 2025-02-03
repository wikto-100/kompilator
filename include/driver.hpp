#ifndef __DRIVER_HPP__
#define __DRIVER_HPP__ 1

#include <string>
#include <cstddef>
#include <istream>
#include <memory>   // for std::shared_ptr

#include "scanner.hpp"
#include "parser.tab.hh"
#include "ast.hpp"  // <-- Bring in the AST definitions (Compiler::AST)

namespace Compiler
{

   class Driver
   {
   public:
      Driver() = default;
      virtual ~Driver();

      void compile(const char *const filenameIn, const char *const filenameOut);

      // Store a pointer to the top-level AST node (ProgramAll).
      inline void setASTRoot(std::shared_ptr<AST::ProgramAll> astRoot)
      {
         this->astRoot = std::move(astRoot);
      }

   private:
      void parse(const char *const filename);
      void parse_helper(std::istream &stream);

      void analyze();
      void generate(const char *const filenameOut);

      /*
         Remove this function after the compiler is complete.
         For now, it can help debug the AST.
      */
      void debugTreeDump();

      Compiler::Scanner *scanner = nullptr;
      Compiler::Parser  *parser  = nullptr;

      // The root of the parsed AST:
      std::shared_ptr<AST::ProgramAll> astRoot = nullptr;
   };

} /* end namespace Compiler */

#endif /* END __DRIVER_HPP__ */
