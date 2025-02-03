/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#pragma once

#include "ast.hpp"
#include <memory>
#include <string>
#include <vector>

namespace Compiler
{

    /**
     * \brief Klasa mapująca adresy pamięci dla symboli w AST
     */
    class MemoryMapper
    {
    public:
        // Główna funkcja mapująca adresy pamięci
        void assignAddresses(std::shared_ptr<AST::ProgramAll> root);
        // Komórki pomocnicze (T1-T9)
        long long T1_ = 0;
        long long T2_ = 0;
        long long T3_ = 0;
        long long T4_ = 0;
        long long T5_ = 0;
        long long T6_ = 0;
        long long T7_ = 0;
        long long T8_ = 0;
        long long T9_ = 0;

        long long computedAddress_RES_ = 0; // Obliczony adres do zapisania wyniku wyrażenia
        long long computedAddress_TMP_ = 0; // Obliczony adres dla tablicy (tymczasowy)

    private:

        // Następny wolny adres pamięci
        long long nextFreeAddress_ = 1;

        // Pzechodzą przez wszystkie procedury i program główny i mapują adresy pamięci
        void mapProcedure(const std::shared_ptr<AST::Procedure> &proc);
        void mapMain(const std::shared_ptr<AST::Main> &mainPart);

        // Mapuje deklaracje lokalne
        void mapLocalDeclarations(const std::vector<std::shared_ptr<AST::Declarations>> &decls);

        // Mapuje pojedynczy symbol
        void mapSymbol(const std::shared_ptr<AST::Symbol> &sym);

        // Przechodzą przez wszystkie komendy i mapują adresy pamięci
        void mapCommands(const std::vector<std::shared_ptr<AST::Command>> &commands);
        void mapCommand(const std::shared_ptr<AST::Command> &cmd);

    };

} // namespace Compiler
