/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#include "sema.hpp"
#include <sstream>
#include <algorithm>

namespace Compiler
{

    bool SemanticChecker::checkProgram(const std::shared_ptr<AST::ProgramAll> &root)
    {
        if (!root)
        {
            reportError("Brak AST do analizy semantycznej.");
            return false;
        }

        pass1ProgramAll(root);

        pass2ProgramAll(root);

        return errors_.empty();
    }

    void SemanticChecker::reportError(const std::string &msg, int line, int col)
    {
        std::ostringstream oss;
        if (line > 0 || col > 0)
        {
            oss << "[Linia " << line << ", Kolumna " << col << "] ";
        }
        oss << msg;
        errors_.push_back(oss.str());
    }

    void SemanticChecker::pass1ProgramAll(const std::shared_ptr<AST::ProgramAll> &root)
    {
        int currentIndex = 0;

        for (auto &procNode : root->procedures)
        {
            const std::string &pname = procNode->name;
            if (procTable_.find(pname) != procTable_.end())
            {
                reportError("Podwójna deklaracja procedury '" + pname + "'.");
            }
            auto record = std::make_shared<ProcedureRecord>();
            record->procNode = procNode;
            record->index = currentIndex++;

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

        for (auto &r : procedureOrder_)
        {
            pass1Procedure(r);
        }

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

        for (auto &argPair : procNode->arguments)
        {
            bool isArrayParam = argPair.first;
            const std::string &paramName = argPair.second;

            bool duplicate = false;
            auto sym = createSymbol(paramName, localSyms, &duplicate);
            if (duplicate)
            {
                continue;
            }
            sym->isArray = isArrayParam;
            sym->isInitialized = true;
            sym->isRefParam = true;
            procNode->paramSymbols.push_back(sym);
        }

        for (auto &declsPtr : procNode->localDeclarations)
        {
            pass1Declarations(declsPtr, localSyms);
        }

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

                continue;
            }

            item.symbol = sym;

            sym->isArray = item.isArray;
            sym->arrayStart = item.rangeStart;
            sym->arrayEnd = item.rangeEnd;
            sym->isInitialized = sym->isArray;

            if (sym->isArray && sym->arrayStart > sym->arrayEnd)
            {
                reportError("Nieprawidłowy zakres tablicy '" + item.name + "' (start > end).");
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

                auto existingSym = findSymbol(cmd->forCmd->loopVar, symList);
                if (existingSym)
                {
                    reportError("Iterator pętli '" + cmd->forCmd->loopVar +
                                "' w konflikcie z istniejącą zmienną.");
                }

                auto loopSym = createSymbol(cmd->forCmd->loopVar, symList);
                loopSym->isIterator = true;
                loopSym->isInitialized = true;
                cmd->forCmd->loopVarSymbol = loopSym;

                pass1Value(cmd->forCmd->startValue, symList);
                pass1Value(cmd->forCmd->endValue, symList);
                pass1Commands(cmd->forCmd->body, symList);

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

    void SemanticChecker::pass1ProcCallCmd(AST::ProcCallCmd &callCmd,
                                           std::vector<std::shared_ptr<AST::Symbol>> &symList)
    {

        auto rec = findProcedureRecord(callCmd.name);
        if (rec)
        {
            callCmd.callee = rec->procNode;
        }
        else
        {
            reportError("Wywołanie nieistniejącej procedury '" + callCmd.name + "'");
        }

        callCmd.argSymbols.clear();
        callCmd.argSymbols.reserve(callCmd.args.size());

        for (auto &argName : callCmd.args)
        {
            auto sym = findSymbol(argName, symList);
            if (!sym)
            {
                reportError("Użycie niezadeklarowanej zmiennej '" + argName + "' w wywołaniu procedury.");

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

    std::shared_ptr<AST::Symbol>
    SemanticChecker::createSymbol(const std::string &name,
                                  std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                  bool *duplicateOut)
    {
        auto existing = findSymbol(name, symList);
        if (existing)
        {
            reportError("Podwójna deklaracja '" + name + "'");
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

    void SemanticChecker::pass2ProgramAll(const std::shared_ptr<AST::ProgramAll> &root)
    {
        if (!root)
            return;

        for (auto &record : procedureOrder_)
        {
            pass2Procedure(record);
        }

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
            pass2Identifier(val->identifier, false, symList, currentProcIndex);
        }
    }

    void SemanticChecker::pass2AssignCmd(const AST::AssignCmd &assign,
                                         std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                         int currentProcIndex)
    {

        pass2Identifier(const_cast<AST::Identifier &>(assign.lhs), true,
                        symList, currentProcIndex);

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

        auto loopSym = forCmd.loopVarSymbol;
        if (!loopSym)
        {
            reportError("Brak symbolu iteratora '" + forCmd.loopVar + "'");
        }
        else
        {

            symList.push_back(loopSym);
        }

        pass2Value(forCmd.startValue, symList, currentProcIndex);
        pass2Value(forCmd.endValue, symList, currentProcIndex);

        pass2Commands(forCmd.body, symList, currentProcIndex);

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
            reportError("Wywołanie niezadeklarowanej procedury '" + procCall.name + "'");
            return;
        }

        auto it = procTable_.find(calleeNode->name);
        if (it == procTable_.end())
        {
            reportError("BŁĄD WEWNĘTRZNY: brak procedury '" + calleeNode->name + "'");
            return;
        }
        auto calleeRec = it->second;
        int calleeIndex = calleeRec->index;
        if (calleeIndex >= callerProcIndex)
        {
            reportError("Wywołanie procedury '" + calleeNode->name + "' w przód lub rekurencyjnie.");
        }

        auto &calleeArgs = calleeNode->arguments;
        auto &callArgs = procCall.args;
        auto &callArgSymbols = procCall.argSymbols;
        if (callArgs.size() != calleeArgs.size())
        {
            reportError("Wywołanie procedury '" + calleeNode->name +
                        "' ma nieprawidłową liczbę argumentów (spodziewano " +
                        std::to_string(calleeArgs.size()) + ", otrzymano " +
                        std::to_string(callArgs.size()) + ")");
            return;
        }

        for (size_t i = 0; i < callArgs.size(); ++i)
        {
            bool paramIsArray = calleeArgs[i].first;
            const std::string &argName = callArgs[i];
            auto sym = callArgSymbols[i];

            if (!sym)
            {

                continue;
            }

            if (sym->isArray != paramIsArray)
            {
                std::string expected = paramIsArray ? "tablicą" : "skalarem";
                std::string actual = sym->isArray ? "tablicą" : "skalarem";
                reportError("Argument '" + argName + "' powinien być " +
                            expected + ", jest " + actual);
            }

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
                pass2Identifier(id, true, symList, currentProcIndex);
            }
        }
        else
        {

            if (ioCmd.writeValue.has_value())
            {
                pass2Value(ioCmd.writeValue.value(), symList, currentProcIndex);
            }
        }
    }

    void SemanticChecker::pass2Identifier(AST::Identifier &id,
                                          bool writing,
                                          std::vector<std::shared_ptr<AST::Symbol>> &symList,
                                          int currentProcIndex)
    {
        auto sym = id.symbol;
        if (!sym)
        {
            reportError("Niezadeklarowana zmienna '" + id.name + "'", id.line, id.column);
            return;
        }

        if (sym->isArray)
        {
            if (id.idxType == AST::IdentifierIndexType::NONE)
            {
                reportError("Tablica '" + id.name + "' użyta jako skalar", id.line, id.column);
            }
            else if (id.idxType == AST::IdentifierIndexType::NUMERIC && !sym->isRefParam)
            {
                long long i = id.indexNumber;
                if (i < sym->arrayStart || i > sym->arrayEnd)
                {
                    reportError("Indeks '" + id.name + "' tablicy poza zakresem",
                                id.line, id.column);
                }
            }
            else if (id.idxType == AST::IdentifierIndexType::VARIABLE)
            {

                auto subSym = id.indexVarSymbol;
                if (!subSym)
                {
                    reportError("Zmienna indeksu tablicy '" + id.indexVar + "' niezadeklarowana",
                                id.line, id.column);
                }
                else
                {

                    AST::Identifier subId;
                    subId.name = id.indexVar;
                    subId.line = id.line;
                    subId.column = id.column;
                    subId.symbol = subSym;

                    pass2Identifier(subId, false, symList, currentProcIndex);
                }
            }
        }
        else
        {

            if (id.idxType != AST::IdentifierIndexType::NONE)
            {
                reportError("Skalar '" + id.name + "' użyty jako tablica", id.line, id.column);
            }
        }

        if (!sym->isArray && !writing && !sym->isInitialized)
        {
            reportError("Użyto niezainicjalizowanej zmiennej '" + id.name + "'",
                        id.line, id.column);
        }

        if (writing && !sym->isArray)
        {
            sym->isInitialized = true;
        }

        if (sym->isIterator && writing)
        {
            reportError("Iterator pętli '" + id.name + "' nie może być modfikowany",
                        id.line, id.column);
        }
    }

} // namespace Compiler