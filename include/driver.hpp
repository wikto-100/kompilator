
#ifndef __DRIVER_HPP__
#define __DRIVER_HPP__ 1
/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#include <string>
#include <cstddef>
#include <istream>
#include <memory>

#include "scanner.hpp"
#include "parser.tab.hh"
#include "ast.hpp"

namespace Compiler
{
   /**
    * \brief Klasa koordynująca proces kompilacji.
    */
   class Driver
   {
   public:
      Driver() = default;
      virtual ~Driver();
      // Funkcja kompilująca plik wejściowy do pliku wyjściowego.
      void compile(const char *const filenameIn, const char *const filenameOut);

      // Funkcje dostępu do AST
      inline void setASTRoot(std::shared_ptr<AST::ProgramAll> astRoot)
      {
         this->astRoot = std::move(astRoot);
      }

   private:
      // Funkcja parsera dla pliku wejściowego
      void parse(const char *const filename);

      // Funkcja pomocnicza dla parsera
      void parse_helper(std::istream &stream);

      // Funkcja analizy semantycznej
      void analyze();

      // Funkcja generująca kod
      void generate(const char *const filenameOut);

      // Skaner i parser
      Compiler::Scanner *scanner = nullptr;
      Compiler::Parser *parser = nullptr;

      // Drzewo AST
      std::shared_ptr<AST::ProgramAll> astRoot = nullptr;
   };

} // namespace Compiler

#endif /* END __DRIVER_HPP__ */
