#include "mem.hpp"

namespace Compiler
{

    // -------------------------------------------------
    // Public Entry Point
    // -------------------------------------------------
    void MemoryMapper::assignAddresses(std::shared_ptr<AST::ProgramAll> root)
    {
        if (!root)
            return;

        //
        // --- Phase A: Map top-level declarations ---
        //
        // 1) Map each procedure’s labelProc, return-addr cell, parameters, local declarations
        for (auto &proc : root->procedures)
        {
            mapProcedure(proc);
        }

        // 2) Map the main part (global declarations)
        if (root->mainPart)
        {
            mapMain(root->mainPart);
        }

        //
        // --- Phase B: Traverse all commands to:
        //       * map the loop variables in ForCmd (forCmd->loopVarSymbol)
        //       * assign labels for IF/WHILE/REPEAT/FOR
        //
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

    // -------------------------------------------------
    // Phase A: map a single Procedure
    // -------------------------------------------------
    void MemoryMapper::mapProcedure(const std::shared_ptr<AST::Procedure> &proc)
    {
        if (!proc)
            return;



        // 2) Reserve one cell for this procedure's return address
        //    => when we call the procedure, we store the caller's IP here
        proc->returnAddrCell = nextFreeAddress_++;

        // 3) If we have a procSymbol, mark it isProcedure
        if (proc->procSymbol)
        {
            proc->procSymbol->isProcedure = true;
            // Typically we don't store a data address for procedures
        }

        // 4) Map parameter symbols (e.g., reference params => 1 cell each)
        for (auto &paramSym : proc->paramSymbols)
        {
            mapSymbol(paramSym);
        }

        // 5) Map local declarations -> each DeclItem has item.symbol
        mapLocalDeclarations(proc->localDeclarations);
    }

    // -------------------------------------------------
    // Phase A: map Main part
    // -------------------------------------------------
    void MemoryMapper::mapMain(const std::shared_ptr<AST::Main> &mainPart)
    {
        if (!mainPart)
            return;
        mapLocalDeclarations(mainPart->declarations);
    }

    // -------------------------------------------------
    // Map local declarations array
    // -------------------------------------------------
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

    // -------------------------------------------------
    // Assign memory for a single symbol
    // -------------------------------------------------
    void MemoryMapper::mapSymbol(const std::shared_ptr<AST::Symbol> &sym)
    {
        if (!sym)
            return;
        if (sym->isProcedure)
            return; // No data memory for procedure symbols
        if (sym->address != 0)
            return; // Already assigned

        // If it's a reference parameter => 1 cell
        if (sym->isRefParam)
        {
            sym->address = nextFreeAddress_++;
            return;
        }

        // If it's an array => pointerCell, offsetCell, then data
        if (sym->isArray)
        {
            long long length = sym->arrayEnd - sym->arrayStart + 1;

            // pointerCell = nextFreeAddress_
            // offsetCell  = nextFreeAddress_ + 1
            // base        = nextFreeAddress_ + 2
            long long offsetCell = nextFreeAddress_;
            long long base = nextFreeAddress_ + 1;

            sym->offsetCell = offsetCell;
            sym->address = base; // base = start of the array data

            nextFreeAddress_ += (length + 1);

            return;
        }

        // Otherwise, a normal scalar => 1 cell
        sym->address = nextFreeAddress_++;
    }

    // -------------------------------------------------
    // Phase B: mapAndLabelCommands
    // -------------------------------------------------
    void MemoryMapper::mapCommands(const std::vector<std::shared_ptr<AST::Command>> &commands)
    {
        for (auto &cmd : commands)
        {
            mapCommand(cmd);
        }
    }

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

                // Recurse into then/else
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
                // 1) Map the loop variable symbol (ensures it has an address)
                if (cmd->forCmd->loopVarSymbol)
                {
                    mapSymbol(cmd->forCmd->loopVarSymbol);
                }
                if (!cmd->forCmd->downTo)
                {
                    // It's an upward FOR.
                    // Allocate 1 new cell to store the countdown
                    cmd->forCmd->helperCell = nextFreeAddress_++;
                }



                // 3) Recurse into the FOR body
                mapCommands(cmd->forCmd->body);
            }
            break;

        default:
            // No label assignment for other commands
            break;
        }
    }


} // namespace Compiler
