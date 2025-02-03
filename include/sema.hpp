#pragma once

#include "ast.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace Compiler {

/**
 * \brief A structure for storing procedure data during Pass1, used in Pass2.
 */
struct ProcedureRecord
{
    std::shared_ptr<AST::Procedure> procNode;
    int index = -1;  ///< The order of declaration for forward-call checks

    // Local symbols for variables/arrays/loop vars
    std::vector<std::shared_ptr<AST::Symbol>> localSymbols;
};

/**
 * \brief Performs semantic analysis in two passes:
 *  Pass1: Build symbol tables & link Identifiers
 *  Pass2: Usage checks
 */
class SemanticChecker {
public:
    bool checkProgram(const std::shared_ptr<AST::ProgramAll> &root);
    const std::vector<std::string>& errors() const { return errors_; }

private:
    void reportError(const std::string &msg, int line=0, int col=0);

    // -------------------------------
    // PASS 1
    // -------------------------------
    void pass1ProgramAll(const std::shared_ptr<AST::ProgramAll> &root);
    void pass1Procedure(std::shared_ptr<ProcedureRecord> &record);
    void pass1Main(const std::shared_ptr<AST::Main> &mainNode);

    void pass1ProcCallCmd(AST::ProcCallCmd &callCmd,
                                           std::vector<std::shared_ptr<AST::Symbol>> &symList);

    void pass1Declarations(const std::shared_ptr<AST::Declarations> &decls,
                           std::vector<std::shared_ptr<AST::Symbol>> &symList);

    // Commands
    void pass1Commands(const std::vector<std::shared_ptr<AST::Command>> &commands,
                       std::vector<std::shared_ptr<AST::Symbol>> &symList);
    void pass1Command(const std::shared_ptr<AST::Command> &cmd,
                      std::vector<std::shared_ptr<AST::Symbol>> &symList);

    // Expressions/Conditions/Values
    void pass1Expression(const std::shared_ptr<AST::Expression> &expr,
                         std::vector<std::shared_ptr<AST::Symbol>> &symList);
    void pass1Condition(const std::shared_ptr<AST::Condition> &cond,
                        std::vector<std::shared_ptr<AST::Symbol>> &symList);
    void pass1Value(const std::shared_ptr<AST::Value> &val,
                    std::vector<std::shared_ptr<AST::Symbol>> &symList);

    // **New** helper to link a single Identifier (e.g., LHS of assign or read target)
    void pass1Identifier(AST::Identifier &id,
                         std::vector<std::shared_ptr<AST::Symbol>> &symList);

    // Symbol creation/find
    std::shared_ptr<AST::Symbol> createSymbol(const std::string &name,
                                              std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                              bool *duplicateOut=nullptr);
    std::shared_ptr<AST::Symbol> findSymbol(const std::string &name,
                                            const std::vector<std::shared_ptr<AST::Symbol>> &symList) const;

    // Procedure lookups
    std::shared_ptr<ProcedureRecord> findProcedureRecord(const std::string &name);

    // For storing all procedures in the order they appear
    std::vector<std::shared_ptr<ProcedureRecord>> procedureOrder_;
    std::unordered_map<std::string, std::shared_ptr<ProcedureRecord>> procTable_;

    // Main's symbols + index
    std::vector<std::shared_ptr<AST::Symbol>> mainSymbols_;
    int mainIndex_ = -1;

    // -------------------------------
    // PASS 2
    // -------------------------------
    void pass2ProgramAll(const std::shared_ptr<AST::ProgramAll> &root);
    void pass2Procedure(std::shared_ptr<ProcedureRecord> &record);
    void pass2Main(const std::shared_ptr<AST::Main> &mainNode);

    void pass2Commands(const std::vector<std::shared_ptr<AST::Command>> &commands,
                       std::vector<std::shared_ptr<AST::Symbol>> &symList,
                       int currentProcIndex);
    void pass2Command(const std::shared_ptr<AST::Command> &cmd,
                      std::vector<std::shared_ptr<AST::Symbol>> &symList,
                      int currentProcIndex);

    std::shared_ptr<AST::Symbol> findLocalSymbol(const std::string &name,
                                                 std::vector<std::shared_ptr<AST::Symbol>> &symList);

    // Expressions/Conditions
    void pass2Expression(const std::shared_ptr<AST::Expression> &expr,
                         std::vector<std::shared_ptr<AST::Symbol>> &symList,
                         int currentProcIndex);
    void pass2Condition(const std::shared_ptr<AST::Condition> &cond,
                        std::vector<std::shared_ptr<AST::Symbol>> &symList,
                        int currentProcIndex);
    void pass2Value(const std::shared_ptr<AST::Value> &val,
                    std::vector<std::shared_ptr<AST::Symbol>> &symList,
                    int currentProcIndex);

    // Commands
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

    // Identifiers
    void pass2Identifier(AST::Identifier &id,
                         bool writing,
                         std::vector<std::shared_ptr<AST::Symbol>> &symList,
                         int currentProcIndex);

private:
    std::vector<std::string> errors_;
};

} // namespace Compiler
