#include <cctype>
#include <fstream>
#include <cassert>
#include <iostream>
#include <cstdlib>

#include "driver.hpp"
#include "sema.hpp"
#include "codegen.hpp"
namespace Compiler
{

   Driver::~Driver()
   {
      delete scanner;
      scanner = nullptr;
      delete parser;
      parser = nullptr;
   }

   void Driver::parse(const char *const filename)
   {
      assert(filename != nullptr);
      std::ifstream in_file(filename);
      if (!in_file.good())
      {
         std::cerr << "Could not open file \"" << filename << "\"\n";
         exit(EXIT_FAILURE);
      }
      parse_helper(in_file);
   }

   void Driver::parse_helper(std::istream &stream)
   {
      delete scanner;
      scanner = nullptr;

      try
      {
         scanner = new Compiler::Scanner(&stream);
      }
      catch (std::bad_alloc &ba)
      {
         std::cerr << "Failed to allocate scanner: (" << ba.what() << "), exiting!!\n";
         exit(EXIT_FAILURE);
      }

      delete parser;
      parser = nullptr;

      try
      {
         parser = new Compiler::Parser((*scanner) /* scanner */,
                                       (*this) /* driver */);
      }
      catch (std::bad_alloc &ba)
      {
         std::cerr << "Failed to allocate parser: (" << ba.what() << "), exiting!!\n";
         exit(EXIT_FAILURE);
      }
      const int accept(0);
      if (parser->parse() != accept)
      {
         std::cerr << "Parse failed!!\n";
      }
   }

   

   void Driver::analyze()
   {
      if (!astRoot)
      {
         std::cerr << "No AST to analyze.\n";
         return;
      }
      // Perform semantic analysis using the stubbed SemanticChecker
      SemanticChecker checker;

      bool ok = checker.checkProgram(astRoot); // run checks

      if (!ok)
      {
         // Print errors and terminate or handle as you wish
         const auto &errs = checker.errors();
         for (auto &msg : errs)
         {
            std::cerr << "SEMANTIC ERROR: " << msg << "\n";
         }
         // If desired, you could do something else here
         // (e.g., throw std::runtime_error or return a status)
         exit(EXIT_FAILURE);
      }

      // If we reach here, no semantic errors were found
      std::cout << "Semantic analysis passed.\n";
   }

void Driver::generate(const char *const filenameOut)
{
    // We assume astRoot was set by the parse step
    if (!astRoot) {
        std::cerr << "No AST to generate code from.\n";
        return;
    }

    CodeGenerator gen;
    gen.generate(astRoot, filenameOut);

}

   void Driver::compile(const char *const filenameIn, const char *const filenameOut)
   {
      parse(filenameIn);
      analyze();       // run semantic checks
      generate(filenameOut);
   }

} // namespace Compiler
