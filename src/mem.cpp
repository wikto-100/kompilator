/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#include "mem.hpp"

namespace Compiler
{

    // Główna funkcja przypisująca adresy pamięci.
    void MemoryMapper::assignAddresses(std::shared_ptr<AST::ProgramAll> root)
    {
        if (!root)
            return;

        // Faza 1: mapowanie deklaracji procedur oraz głównej części.
        for (auto &proc : root->procedures)
        {
            mapProcedure(proc);
        }
        if (root->mainPart)
        {
            mapMain(root->mainPart);
        }

        // Faza 2: mapowanie komend w procedurach i głównej części.
        for (auto &proc : root->procedures)
        {
            if (!proc)
                continue;
            mapCommands(proc->commands);
        }
        if (root->mainPart)
        {
            mapCommands(root->mainPart->commands);
        }

        computedAddress_RES_ = nextFreeAddress_++;
        computedAddress_TMP_ = nextFreeAddress_++;

        T1_ = nextFreeAddress_++;
        T2_ = nextFreeAddress_++;
        T3_ = nextFreeAddress_++;
        T4_ = nextFreeAddress_++;
        T5_ = nextFreeAddress_++;
        T6_ = nextFreeAddress_++;
        T7_ = nextFreeAddress_++;
        T8_ = nextFreeAddress_++;
        T9_ = nextFreeAddress_++;
    }

    // Mapowanie pojedynczej procedury.
    void MemoryMapper::mapProcedure(const std::shared_ptr<AST::Procedure> &proc)
    {
        if (!proc)
            return;

        proc->returnAddrCell = nextFreeAddress_++;

        if (proc->procSymbol)
        {
            proc->procSymbol->isProcedure = true;
        }

        for (auto &paramSym : proc->paramSymbols)
        {
            mapSymbol(paramSym);
        }

        mapLocalDeclarations(proc->localDeclarations);
    }

    // Mapowanie głównej części programu.
    void MemoryMapper::mapMain(const std::shared_ptr<AST::Main> &mainPart)
    {
        if (!mainPart)
            return;
        mapLocalDeclarations(mainPart->declarations);
    }

    // Mapowanie deklaracji lokalnych.
    void MemoryMapper::mapLocalDeclarations(const std::vector<std::shared_ptr<AST::Declarations>> &decls)
    {
        for (auto &decl : decls)
        {
            if (!decl)
                continue;
            for (auto &item : decl->items)
            {
                if (item.symbol)
                {
                    mapSymbol(item.symbol);
                }
            }
        }
    }

    // Przypisanie pamięci dla symbolu.
    void MemoryMapper::mapSymbol(const std::shared_ptr<AST::Symbol> &sym)
    {
        if (!sym)
            return;
        if (sym->isProcedure)
            return; // Brak pamięci danych dla procedur.
        if (sym->address != 0)
            return; // Adres już przypisany.

        if (sym->isRefParam)
        {
            sym->address = nextFreeAddress_++;
            return;
        }

        if (sym->isArray)
        {
            long long length = sym->arrayEnd - sym->arrayStart + 1;
            long long offsetCell = nextFreeAddress_;
            long long base = nextFreeAddress_ + 1;
            sym->offsetCell = offsetCell;
            sym->address = base;
            nextFreeAddress_ += (length + 1);
            return;
        }

        sym->address = nextFreeAddress_++;
    }

    // Mapowanie zbioru komend.
    void MemoryMapper::mapCommands(const std::vector<std::shared_ptr<AST::Command>> &commands)
    {
        for (auto &cmd : commands)
        {
            mapCommand(cmd);
        }
    }

    // Mapowanie pojedynczej komendy.
    void MemoryMapper::mapCommand(const std::shared_ptr<AST::Command> &cmd)
    {
        if (!cmd)
            return;

        using CT = AST::CommandType;
        switch (cmd->type)
        {
        case CT::IF_THEN:
        case CT::IF_THEN_ELSE:
            if (cmd->ifCmd)
            {
                mapCommands(cmd->ifCmd->thenCommands);
                mapCommands(cmd->ifCmd->elseCommands);
            }
            break;

        case CT::WHILE:
            if (cmd->whileCmd)
            {
                mapCommands(cmd->whileCmd->body);
            }
            break;

        case CT::REPEAT:
            if (cmd->repeatCmd)
            {
                mapCommands(cmd->repeatCmd->body);
            }
            break;

        case CT::FOR_TO:
        case CT::FOR_DOWNTO:
            if (cmd->forCmd)
            {
                if (cmd->forCmd->loopVarSymbol)
                {
                    mapSymbol(cmd->forCmd->loopVarSymbol);
                }
                if (!cmd->forCmd->downTo)
                {
                    cmd->forCmd->helperCell = nextFreeAddress_++;
                }
                mapCommands(cmd->forCmd->body);
            }
            break;

        default:
            break;
        }
    }

} // namespace Compiler
