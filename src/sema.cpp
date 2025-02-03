#include "sema.hpp"
#include <sstream>
#include <algorithm>

namespace Compiler
{

    bool SemanticChecker::checkProgram(const std::shared_ptr<AST::ProgramAll> &root)
    {
        if (!root)
        {
            reportError("No AST to check.");
            return false;
        }

        // Pass1: Build & link
        pass1ProgramAll(root);

        // Pass2: Usage checks
        pass2ProgramAll(root);

        return errors_.empty();
    }

    void SemanticChecker::reportError(const std::string &msg, int line, int col)
    {
        std::ostringstream oss;
        if (line > 0 || col > 0)
        {
            oss << "[Line " << line << ", Col " << col << "] ";
        }
        oss << msg;
        errors_.push_back(oss.str());
    }

    /* ------------------------------------------------------------------
       PASS 1: Build & Link
    ------------------------------------------------------------------ */

    void SemanticChecker::pass1ProgramAll(const std::shared_ptr<AST::ProgramAll> &root)
    {
        int currentIndex = 0;
        // 1) Procedures
        for (auto &procNode : root->procedures)
        {
            const std::string &pname = procNode->name;
            if (procTable_.find(pname) != procTable_.end())
            {
                reportError("Duplicate procedure '" + pname + "' declared.");
            }
            auto record = std::make_shared<ProcedureRecord>();
            record->procNode = procNode;
            record->index = currentIndex++;

            // If the procedure node doesn't have a symbol
            if (!procNode->procSymbol)
            {
                auto sym = std::make_shared<AST::Symbol>();
                sym->name = pname;
                sym->isProcedure = true;
                procNode->procSymbol = sym;
            }

            procTable_[pname] = record;
            procedureOrder_.push_back(record);
        }

        // 2) Each procedure
        for (auto &r : procedureOrder_)
        {
            pass1Procedure(r);
        }

        // 3) Main
        mainIndex_ = currentIndex;
        if (root->mainPart)
        {
            pass1Main(root->mainPart);
        }
    }

    void SemanticChecker::pass1Procedure(std::shared_ptr<ProcedureRecord> &record)
    {
        auto procNode = record->procNode;
        auto &localSyms = record->localSymbols;

        // 1) Parameters
        for (auto &argPair : procNode->arguments)
        {
            bool isArrayParam = argPair.first;
            const std::string &paramName = argPair.second;

            bool duplicate = false;
            auto sym = createSymbol(paramName, localSyms, &duplicate);
            if (duplicate)
            {
                continue; // error reported
            }
            sym->isArray = isArrayParam;
            sym->isInitialized = true; // parameters are "initialized"
            sym->isRefParam = true;    // parameters are passed by reference
            procNode->paramSymbols.push_back(sym);
        }

        // 2) Local declarations
        for (auto &declsPtr : procNode->localDeclarations)
        {
            pass1Declarations(declsPtr, localSyms);
        }

        // 3) Commands (includes linking calls, references, etc.)
        pass1Commands(procNode->commands, localSyms);
    }

    void SemanticChecker::pass1Main(const std::shared_ptr<AST::Main> &mainNode)
    {
        for (auto &declPtr : mainNode->declarations)
        {
            pass1Declarations(declPtr, mainSymbols_);
        }
        pass1Commands(mainNode->commands, mainSymbols_);
    }

    void SemanticChecker::pass1Declarations(const std::shared_ptr<AST::Declarations> &decls,
                                            std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {
        if (!decls)
            return;

        for (auto &item : decls->items)
        {
            bool duplicate = false;
            auto sym = createSymbol(item.name, symList, &duplicate);
            if (duplicate)
            {
                // We already reported an error for the duplicate,
                // so we skip any further initialization for this item.
                continue;
            }

            // Attach the newly created symbol to the DeclItem
            item.symbol = sym;

            // Copy over fields
            sym->isArray = item.isArray;
            sym->arrayStart = item.rangeStart;
            sym->arrayEnd = item.rangeEnd;
            sym->isInitialized = sym->isArray; // arrays start as "initialized"

            // Check bounds
            if (sym->isArray && sym->arrayStart > sym->arrayEnd)
            {
                reportError("Invalid array bounds for '" + item.name + "' (start > end).");
            }
        }
    }

    void SemanticChecker::pass1Commands(const std::vector<std::shared_ptr<AST::Command>> &commands,
                                        std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {
        for (auto &cmd : commands)
        {
            pass1Command(cmd, symList);
        }
    }

    void SemanticChecker::pass1Command(const std::shared_ptr<AST::Command> &cmd,
                                       std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {
        if (!cmd)
            return;
        using CT = AST::CommandType;

        switch (cmd->type)
        {
        case CT::ASSIGN:
            if (cmd->assignCmd)
            {
                // Link LHS
                pass1Identifier(cmd->assignCmd->lhs, symList);
                pass1Expression(cmd->assignCmd->rhs, symList);
            }
            break;
        case CT::IF_THEN:
        case CT::IF_THEN_ELSE:
            if (cmd->ifCmd)
            {
                pass1Condition(cmd->ifCmd->condition, symList);
                pass1Commands(cmd->ifCmd->thenCommands, symList);
                pass1Commands(cmd->ifCmd->elseCommands, symList);
            }
            break;
        case CT::WHILE:
            if (cmd->whileCmd)
            {
                pass1Condition(cmd->whileCmd->condition, symList);
                pass1Commands(cmd->whileCmd->body, symList);
            }
            break;
        case CT::REPEAT:
            if (cmd->repeatCmd)
            {
                pass1Commands(cmd->repeatCmd->body, symList);
                pass1Condition(cmd->repeatCmd->condition, symList);
            }
            break;
        case CT::FOR_TO:
        case CT::FOR_DOWNTO:
            if (cmd->forCmd)
            {
                cmd->forCmd->downTo = (cmd->type == CT::FOR_DOWNTO);
                // Check if loop var conflicts
                auto existingSym = findSymbol(cmd->forCmd->loopVar, symList);
                if (existingSym)
                {
                    reportError("Loop iterator '" + cmd->forCmd->loopVar +
                                "' conflicts with existing declaration.");
                }
                // Create loop var symbol
                auto loopSym = createSymbol(cmd->forCmd->loopVar, symList);
                loopSym->isIterator = true;
                loopSym->isInitialized = true;
                cmd->forCmd->loopVarSymbol = loopSym;

                pass1Value(cmd->forCmd->startValue, symList);
                pass1Value(cmd->forCmd->endValue, symList);
                pass1Commands(cmd->forCmd->body, symList);

                // Remove if language says scope ends
                auto it = std::remove(symList.begin(), symList.end(), loopSym);
                if (it != symList.end())
                {
                    symList.erase(it, symList.end());
                }
            }
            break;
        case CT::PROC_CALL:
            if (cmd->procCallCmd)
            {
                pass1ProcCallCmd(*cmd->procCallCmd, symList);
            }
            break;
        case CT::READ:
            if (cmd->ioCmd && cmd->ioCmd->readTarget.has_value())
            {
                pass1Identifier(cmd->ioCmd->readTarget.value(), symList);
            }
            break;
        case CT::WRITE:
            if (cmd->ioCmd && cmd->ioCmd->writeValue.has_value())
            {
                pass1Value(cmd->ioCmd->writeValue.value(), symList);
            }
            break;
        }
    }

    // NEW helper: Link procedure call arguments to symbols
    void SemanticChecker::pass1ProcCallCmd(AST::ProcCallCmd &callCmd,
                                           std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {
        // 1) Link to procedure if it exists
        auto rec = findProcedureRecord(callCmd.name);
        if (rec)
        {
            callCmd.callee = rec->procNode;
        }
        else
        {
            reportError("Call to undeclared procedure '" + callCmd.name + "'");
        }

        // 2) For each argument name => find the symbol in this caller scope
        callCmd.argSymbols.clear();
        callCmd.argSymbols.reserve(callCmd.args.size());

        for (auto &argName : callCmd.args)
        {
            auto sym = findSymbol(argName, symList);
            if (!sym)
            {
                reportError("Use of undeclared variable '" + argName + "' in procedure call");
                // push back a null so indices align
                callCmd.argSymbols.push_back(nullptr);
            }
            else
            {
                callCmd.argSymbols.push_back(sym);
            }
        }
    }

    void SemanticChecker::pass1Expression(const std::shared_ptr<AST::Expression> &expr,
                                          std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {
        if (!expr)
            return;
        pass1Value(expr->left, symList);
        if (expr->op != AST::ExprOp::NONE && expr->right)
        {
            pass1Value(expr->right, symList);
        }
    }

    void SemanticChecker::pass1Condition(const std::shared_ptr<AST::Condition> &cond,
                                         std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {
        if (!cond)
            return;
        pass1Value(cond->left, symList);
        pass1Value(cond->right, symList);
    }

    void SemanticChecker::pass1Value(const std::shared_ptr<AST::Value> &val,
                                     std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {
        if (!val)
            return;
        if (val->isIdentifier)
        {
            pass1Identifier(val->identifier, symList);
        }
    }

    // link a single Identifier
    void SemanticChecker::pass1Identifier(AST::Identifier &id,
                                          std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {
        auto sym = findSymbol(id.name, symList);
        if (sym)
        {
            id.symbol = sym;
        }

        if (id.idxType == AST::IdentifierIndexType::VARIABLE)
        {
            auto subSym = findSymbol(id.indexVar, symList);
            if (subSym)
            {
                id.indexVarSymbol = subSym;
            }
        }
    }

    /* ------------------------------------------------------------------
       Symbol creation & lookups
    ------------------------------------------------------------------ */

    std::shared_ptr<AST::Symbol>
    SemanticChecker::createSymbol(const std::string &name,
                                  std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                  bool *duplicateOut)
    {
        auto existing = findSymbol(name, symList);
        if (existing)
        {
            reportError("Duplicate declaration of '" + name + "'");
            if (duplicateOut)
                *duplicateOut = true;
            return existing;
        }
        auto newSym = std::make_shared<AST::Symbol>();
        newSym->name = name;
        symList.push_back(newSym);
        if (duplicateOut)
            *duplicateOut = false;
        return newSym;
    }

    std::shared_ptr<AST::Symbol>
    SemanticChecker::findSymbol(const std::string &name,
                                const std::vector<std::shared_ptr<AST::Symbol>> &symList) const
    {
        for (auto &s : symList)
        {
            if (s->name == name)
            {
                return s;
            }
        }
        return nullptr;
    }

    std::shared_ptr<ProcedureRecord>
    SemanticChecker::findProcedureRecord(const std::string &name)
    {
        auto it = procTable_.find(name);
        if (it == procTable_.end())
        {
            return nullptr;
        }
        return it->second;
    }

    /* ------------------------------------------------------------------
       PASS 2
    ------------------------------------------------------------------ */

    void SemanticChecker::pass2ProgramAll(const std::shared_ptr<AST::ProgramAll> &root)
    {
        if (!root)
            return;

        // For each procedure in declared order
        for (auto &record : procedureOrder_)
        {
            pass2Procedure(record);
        }

        // Then main
        if (root->mainPart)
        {
            pass2Main(root->mainPart);
        }
    }

    void SemanticChecker::pass2Procedure(std::shared_ptr<ProcedureRecord> &record)
    {
        auto &procNode = record->procNode;
        auto &localSyms = record->localSymbols;
        int procIndex = record->index;

        pass2Commands(procNode->commands, localSyms, procIndex);
    }

    void SemanticChecker::pass2Main(const std::shared_ptr<AST::Main> &mainNode)
    {
        pass2Commands(mainNode->commands, mainSymbols_, mainIndex_);
    }

    void SemanticChecker::pass2Commands(const std::vector<std::shared_ptr<AST::Command>> &commands,
                                        std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                        int currentProcIndex)
    {
        for (auto &cmd : commands)
        {
            pass2Command(cmd, symList, currentProcIndex);
        }
    }

    void SemanticChecker::pass2Command(const std::shared_ptr<AST::Command> &cmd,
                                       std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                       int currentProcIndex)
    {
        if (!cmd)
            return;
        using CT = AST::CommandType;

        switch (cmd->type)
        {
        case CT::ASSIGN:
            if (cmd->assignCmd)
            {
                pass2AssignCmd(*cmd->assignCmd, symList, currentProcIndex);
            }
            break;
        case CT::IF_THEN:
        case CT::IF_THEN_ELSE:
            if (cmd->ifCmd)
            {
                pass2IfCmd(*cmd->ifCmd, symList, currentProcIndex);
            }
            break;
        case CT::WHILE:
            if (cmd->whileCmd)
            {
                pass2WhileCmd(*cmd->whileCmd, symList, currentProcIndex);
            }
            break;
        case CT::REPEAT:
            if (cmd->repeatCmd)
            {
                pass2RepeatCmd(*cmd->repeatCmd, symList, currentProcIndex);
            }
            break;
        case CT::FOR_TO:
        case CT::FOR_DOWNTO:
            if (cmd->forCmd)
            {
                pass2ForCmd(*cmd->forCmd, symList, currentProcIndex);
            }
            break;
        case CT::PROC_CALL:
            if (cmd->procCallCmd)
            {
                pass2ProcCallCmd(*cmd->procCallCmd, currentProcIndex);
            }
            break;
        case CT::READ:
        case CT::WRITE:
            if (cmd->ioCmd)
            {
                pass2IOCmd(*cmd->ioCmd, cmd->type, symList, currentProcIndex);
            }
            break;
        }
    }

    /* ------------------------------------------------------------------
       Helper for pass2 local symbol lookup
    ------------------------------------------------------------------ */
    std::shared_ptr<AST::Symbol>
    SemanticChecker::findLocalSymbol(const std::string &name,
                                     std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {
        for (auto &s : symList)
        {
            if (s->name == name)
            {
                return s;
            }
        }
        return nullptr;
    }

    /* ------------------------------------------------------------------
       PASS2: Expressions / Conditions
    ------------------------------------------------------------------ */

    void SemanticChecker::pass2Expression(const std::shared_ptr<AST::Expression> &expr,
                                          std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                          int currentProcIndex)
    {
        if (!expr)
            return;
        pass2Value(expr->left, symList, currentProcIndex);
        if (expr->op != AST::ExprOp::NONE && expr->right)
        {
            pass2Value(expr->right, symList, currentProcIndex);
        }
    }

    void SemanticChecker::pass2Condition(const std::shared_ptr<AST::Condition> &cond,
                                         std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                         int currentProcIndex)
    {
        if (!cond)
            return;
        pass2Value(cond->left, symList, currentProcIndex);
        pass2Value(cond->right, symList, currentProcIndex);
    }

    void SemanticChecker::pass2Value(const std::shared_ptr<AST::Value> &val,
                                     std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                     int currentProcIndex)
    {
        if (!val)
            return;
        if (val->isIdentifier)
        {
            pass2Identifier(val->identifier, /*writing=*/false, symList, currentProcIndex);
        }
    }

    /* ------------------------------------------------------------------
       PASS2: Command-specific checks
    ------------------------------------------------------------------ */

    void SemanticChecker::pass2AssignCmd(const AST::AssignCmd &assign,
                                         std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                         int currentProcIndex)
    {
        // LHS => writing
        pass2Identifier(const_cast<AST::Identifier &>(assign.lhs), /*writing=*/true,
                        symList, currentProcIndex);

        // RHS
        pass2Expression(assign.rhs, symList, currentProcIndex);
    }

    void SemanticChecker::pass2IfCmd(const AST::IfCmd &ifCmd,
                                     std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                     int currentProcIndex)
    {
        pass2Condition(ifCmd.condition, symList, currentProcIndex);
        pass2Commands(ifCmd.thenCommands, symList, currentProcIndex);
        pass2Commands(ifCmd.elseCommands, symList, currentProcIndex);
    }

    void SemanticChecker::pass2WhileCmd(const AST::WhileCmd &whileCmd,
                                        std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                        int currentProcIndex)
    {
        pass2Condition(whileCmd.condition, symList, currentProcIndex);
        pass2Commands(whileCmd.body, symList, currentProcIndex);
    }

    void SemanticChecker::pass2RepeatCmd(const AST::RepeatCmd &repeatCmd,
                                         std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                         int currentProcIndex)
    {
        pass2Commands(repeatCmd.body, symList, currentProcIndex);
        pass2Condition(repeatCmd.condition, symList, currentProcIndex);
    }

    void SemanticChecker::pass2ForCmd(const AST::ForCmd &forCmd,
                                      std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                      int currentProcIndex)
    {
        // 1) Push the loop variable back into the scope
        auto loopSym = forCmd.loopVarSymbol;
        if (!loopSym)
        {
            reportError("Missing loop variable symbol for '" + forCmd.loopVar + "'");
        }
        else
        {
            // Insert at the end of the vector to simulate scope
            symList.push_back(loopSym);
        }

        // 2) Check the start/end expressions
        pass2Value(forCmd.startValue, symList, currentProcIndex);
        pass2Value(forCmd.endValue, symList, currentProcIndex);

        // 3) Analyze the body
        pass2Commands(forCmd.body, symList, currentProcIndex);

        // 4) Pop the loop variable from scope
        if (loopSym)
        {
            auto it = std::remove(symList.begin(), symList.end(), loopSym);
            symList.erase(it, symList.end());
        }
    }

    void SemanticChecker::pass2ProcCallCmd(const AST::ProcCallCmd &procCall,
                                           int callerProcIndex)
    {
        auto calleeNode = procCall.callee;
        if (!calleeNode)
        {
            reportError("Call to undeclared procedure '" + procCall.name + "'");
            return;
        }
        // Disallow forward/recursive calls
        auto it = procTable_.find(calleeNode->name);
        if (it == procTable_.end())
        {
            reportError("Internal error: no record found for procedure '" + calleeNode->name + "'");
            return;
        }
        auto calleeRec = it->second;
        int calleeIndex = calleeRec->index;
        if (calleeIndex >= callerProcIndex)
        {
            reportError("Call to procedure '" + calleeNode->name + "' is forward or recursive, not allowed.");
        }

        // Compare argument counts
        auto &calleeArgs = calleeNode->arguments;
        auto &callArgs = procCall.args;
        auto &callArgSymbols = procCall.argSymbols; // Now we can read from here
        if (callArgs.size() != calleeArgs.size())
        {
            reportError("Call to procedure '" + calleeNode->name +
                        "' has wrong number of arguments (expected " +
                        std::to_string(calleeArgs.size()) + ", got " +
                        std::to_string(callArgs.size()) + ")");
            return;
        }

        // Check array/scalar match
        for (size_t i = 0; i < callArgs.size(); ++i)
        {
            bool paramIsArray = calleeArgs[i].first;
            const std::string &argName = callArgs[i];
            auto sym = callArgSymbols[i]; // we set this in pass1ProcCallCmd

            if (!sym)
            {
                // We previously reported error for this argument
                continue;
            }

            if (sym->isArray != paramIsArray)
            {
                std::string expected = paramIsArray ? "array" : "scalar";
                std::string actual = sym->isArray ? "array" : "scalar";
                reportError("Argument '" + argName + "' type mismatch: expected " +
                            expected + ", got " + actual);
            }
            // Mark as used => if the language means read usage
            sym->isInitialized = true; 
        }
    }

    void SemanticChecker::pass2IOCmd(const AST::IOCommand &ioCmd,
                                     AST::CommandType cmdType,
                                     std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                     int currentProcIndex)
    {
        if (ioCmd.ioType == AST::IOType::READ)
        {
            if (ioCmd.readTarget.has_value())
            {
                auto &id = const_cast<AST::Identifier &>(ioCmd.readTarget.value());
                pass2Identifier(id, /*writing=*/true, symList, currentProcIndex);
            }
        }
        else
        {
            // WRITE
            if (ioCmd.writeValue.has_value())
            {
                pass2Value(ioCmd.writeValue.value(), symList, currentProcIndex);
            }
        }
    }

    /* ------------------------------------------------------------------
       PASS2: Identifier check
    ------------------------------------------------------------------ */

    void SemanticChecker::pass2Identifier(AST::Identifier &id,
                                          bool writing,
                                          std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                          int currentProcIndex)
    {
        auto sym = id.symbol;
        if (!sym)
        {
            reportError("Undeclared variable '" + id.name + "'", id.line, id.column);
            return;
        }

        if (sym->isArray)
        {
            if (id.idxType == AST::IdentifierIndexType::NONE)
            {
                reportError("Array '" + id.name + "' used as scalar", id.line, id.column);
            }
            else if (id.idxType == AST::IdentifierIndexType::NUMERIC && !sym->isRefParam)
            {
                long long i = id.indexNumber;
                if (i < sym->arrayStart || i > sym->arrayEnd)
                {
                    reportError("Array index out of bounds for '" + id.name + "'",
                                id.line, id.column);
                }
            }
            else if (id.idxType == AST::IdentifierIndexType::VARIABLE)
            {
                // Look up the subscript variable
                auto subSym = id.indexVarSymbol; 
                if (!subSym)
                {
                    reportError("Array index variable '" + id.indexVar + "' not declared",
                                id.line, id.column);
                }
                else
                {
                    // A subscript is effectively read usage => writing=false
                    AST::Identifier subId;
                    subId.name = id.indexVar;
                    subId.line = id.line;
                    subId.column = id.column;
                    subId.symbol = subSym;

                    pass2Identifier(subId, /*writing=*/false, symList, currentProcIndex);
                }
            }
        }
        else
        {
            // Scalar => must not have an index
            if (id.idxType != AST::IdentifierIndexType::NONE)
            {
                reportError("Scalar '" + id.name + "' used as array", id.line, id.column);
            }
        }

        // Check uninitialized usage
        if (!sym->isArray && !writing && !sym->isInitialized)
        {
            reportError("Using uninitialized variable '" + id.name + "'",
                        id.line, id.column);
        }

        // Mark as initialized if writing
        if (writing && !sym->isArray)
        {
            sym->isInitialized = true;
        }

        // Loop var immutability
        if (sym->isIterator && writing)
        {
            reportError("Loop variable '" + id.name + "' cannot be modified",
                        id.line, id.column);
        }
    }

} // end namespace Compiler
