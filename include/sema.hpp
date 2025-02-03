/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#pragma once

#include "ast.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace Compiler
{

    /**
     * \brief Struktura przechowująca informacje o procedurze
     */
    struct ProcedureRecord
    {
        std::shared_ptr<AST::Procedure> procNode; // Węzeł AST dla procedury
        int index = -1;                           // Liniowy porządek deklaracji procedur

        // Lokalne symbole procedury
        std::vector<std::shared_ptr<AST::Symbol>> localSymbols;
    };

    /**
     * \brief Wykonuje analizę semantyczną na drzewie AST.
     *  Pass1: Buduje symbole, linkuje procedury
     *  Pass2: Sprawdza użycie symboli
     */
    class SemanticChecker
    {
    public:
        // Sprawdza program
        bool checkProgram(const std::shared_ptr<AST::ProgramAll> &root);
        // Zwraca listę błędów
        const std::vector<std::string> &errors() const { return errors_; }

    private:
        // Zgłasza błąd semantyczny
        void reportError(const std::string &msg, int line = 0, int col = 0);

        // -------------------------------
        // PASS 1
        // -------------------------------

        // Program i procedury
        void pass1ProgramAll(const std::shared_ptr<AST::ProgramAll> &root);
        void pass1Procedure(std::shared_ptr<ProcedureRecord> &record);
        void pass1Main(const std::shared_ptr<AST::Main> &mainNode);
        void pass1ProcCallCmd(AST::ProcCallCmd &callCmd,
                              std::vector<std::shared_ptr<AST::Symbol>> &symList);
        void pass1Declarations(const std::shared_ptr<AST::Declarations> &decls,
                               std::vector<std::shared_ptr<AST::Symbol>> &symList);
        // Polecenia
        void pass1Commands(const std::vector<std::shared_ptr<AST::Command>> &commands,
                           std::vector<std::shared_ptr<AST::Symbol>> &symList);
        void pass1Command(const std::shared_ptr<AST::Command> &cmd,
                          std::vector<std::shared_ptr<AST::Symbol>> &symList);

        // Wyrażenia/Warunki/Wartości
        void pass1Expression(const std::shared_ptr<AST::Expression> &expr,
                             std::vector<std::shared_ptr<AST::Symbol>> &symList);
        void pass1Condition(const std::shared_ptr<AST::Condition> &cond,
                            std::vector<std::shared_ptr<AST::Symbol>> &symList);
        void pass1Value(const std::shared_ptr<AST::Value> &val,
                        std::vector<std::shared_ptr<AST::Symbol>> &symList);

        // Łączenie identyfikatorów
        void pass1Identifier(AST::Identifier &id,
                             std::vector<std::shared_ptr<AST::Symbol>> &symList);

        // Tworzenie i wyszukiwanie symboli
        std::shared_ptr<AST::Symbol> createSymbol(const std::string &name,
                                                  std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                                  bool *duplicateOut = nullptr);
        std::shared_ptr<AST::Symbol> findSymbol(const std::string &name,
                                                const std::vector<std::shared_ptr<AST::Symbol>> &symList) const;

        // Wyszukiwanie procedur w liście procedur
        std::shared_ptr<ProcedureRecord> findProcedureRecord(const std::string &name);

        // Wektor przechowujący kolejność deklaracji procedur
        std::vector<std::shared_ptr<ProcedureRecord>> procedureOrder_;
        // Tabela przechoywująca procedury po nazwie
        std::unordered_map<std::string, std::shared_ptr<ProcedureRecord>> procTable_;

        // Symbole programu głównego
        std::vector<std::shared_ptr<AST::Symbol>> mainSymbols_;
        // Indeks procedury głównej (najczęściej ostatni)
        int mainIndex_ = -1;

        // -------------------------------
        // PASS 2
        // -------------------------------

        // Program i procedury
        void pass2ProgramAll(const std::shared_ptr<AST::ProgramAll> &root);
        void pass2Procedure(std::shared_ptr<ProcedureRecord> &record);
        void pass2Main(const std::shared_ptr<AST::Main> &mainNode);
        // Polecenia
        void pass2Commands(const std::vector<std::shared_ptr<AST::Command>> &commands,
                           std::vector<std::shared_ptr<AST::Symbol>> &symList,
                           int currentProcIndex);
        void pass2Command(const std::shared_ptr<AST::Command> &cmd,
                          std::vector<std::shared_ptr<AST::Symbol>> &symList,
                          int currentProcIndex);
        std::shared_ptr<AST::Symbol> findLocalSymbol(const std::string &name,
                                                     std::vector<std::shared_ptr<AST::Symbol>> &symList);

        // Wyrażenia/Warunki/Wartości
        void pass2Expression(const std::shared_ptr<AST::Expression> &expr,
                             std::vector<std::shared_ptr<AST::Symbol>> &symList,
                             int currentProcIndex);
        void pass2Condition(const std::shared_ptr<AST::Condition> &cond,
                            std::vector<std::shared_ptr<AST::Symbol>> &symList,
                            int currentProcIndex);
        void pass2Value(const std::shared_ptr<AST::Value> &val,
                        std::vector<std::shared_ptr<AST::Symbol>> &symList,
                        int currentProcIndex);

        // Polecenia
        void pass2AssignCmd(const AST::AssignCmd &assign,
                            std::vector<std::shared_ptr<AST::Symbol>> &symList,
                            int currentProcIndex);
        void pass2IfCmd(const AST::IfCmd &ifCmd,
                        std::vector<std::shared_ptr<AST::Symbol>> &symList,
                        int currentProcIndex);
        void pass2WhileCmd(const AST::WhileCmd &whileCmd,
                           std::vector<std::shared_ptr<AST::Symbol>> &symList,
                           int currentProcIndex);
        void pass2RepeatCmd(const AST::RepeatCmd &repeatCmd,
                            std::vector<std::shared_ptr<AST::Symbol>> &symList,
                            int currentProcIndex);
        void pass2ForCmd(const AST::ForCmd &forCmd,
                         std::vector<std::shared_ptr<AST::Symbol>> &symList,
                         int currentProcIndex);
        void pass2ProcCallCmd(const AST::ProcCallCmd &procCall,
                              int callerProcIndex);
        void pass2IOCmd(const AST::IOCommand &ioCmd,
                        AST::CommandType cmdType,
                        std::vector<std::shared_ptr<AST::Symbol>> &symList,
                        int currentProcIndex);

        // Identyfikatory
        void pass2Identifier(AST::Identifier &id,
                             bool writing,
                             std::vector<std::shared_ptr<AST::Symbol>> &symList,
                             int currentProcIndex);

    private:
        // Lista błędów
        std::vector<std::string> errors_;
    };

} // namespace Compiler
