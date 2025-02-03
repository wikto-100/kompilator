/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
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
         std::cerr << "Nie można otworzyć pliku \"" << filename << "\"\n";
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
         std::cerr << "Błąd alokacji skanera: (" << ba.what() << ")\n";
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
         std::cerr << "Błąd alokacji parsera: (" << ba.what() << ")\n";
         exit(EXIT_FAILURE);
      }
      const int accept(0);
      if (parser->parse() != accept)
      {
         std::cerr << "Parsowanie: BŁĄD.\n";
      }
   }

   

   void Driver::analyze()
   {
      if (!astRoot)
      {
         std::cerr << "Brak AST do analizy semantycznej.\n";
         return;
      }
      SemanticChecker checker;

      bool ok = checker.checkProgram(astRoot);

      if (!ok)
      {
         const auto &errs = checker.errors();
         for (auto &msg : errs)
         {
            std::cerr << "BŁĄD SEMANTYCZNY: " << msg << "\n";
         }
         std::cerr << "Analiza semantyczna: BŁĄD.\n";
         exit(EXIT_FAILURE);
      }

      std::cout << "Analiza semantyczna: OK.\n";
   }

void Driver::generate(const char *const filenameOut)
{
    // AST powinno być już zbudowane i sprawdzone
    if (!astRoot) {
        std::cerr << "Brak AST do generowania kodu.\n";
        return;
    }

    CodeGenerator gen;
    gen.generate(astRoot, filenameOut);

}

   void Driver::compile(const char *const filenameIn, const char *const filenameOut)
   {
      parse(filenameIn);
      analyze();
      generate(filenameOut);
   }

} // namespace Compiler
