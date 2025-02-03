#pragma once

#include "ast.hpp"
#include <memory>
#include <string>
#include <vector>

namespace Compiler
{

    /**
     * \brief MemoryMapper:
     *  1) Assigns memory addresses to procedure parameters, local variables, arrays, etc.
     *  
     *
     * This implementation uses a two-phase approach:
     *   - Phase A: Map top-level procedures/main, local declarations, parameters.
     *   - Phase B: Traverse commands to assign addresses to loop variables (ForCmd)
     *              
     */
    class MemoryMapper
    {
    public:
        /**
         * \brief Assign addresses to all symbols in the given AST
         *
         */
        void assignAddresses(std::shared_ptr<AST::ProgramAll> root);
                    // scratch cells
        long long T1_ = 0;
        long long T2_ = 0;
        long long T3_ = 0;
        long long T4_ = 0;
        long long T5_ = 0;
        long long T6_ = 0;
        long long T7_ = 0;
        long long T8_ = 0;
        long long T9_ = 0;

        long long computedAddress_RES_ = 0; // for the computed address
        long long computedAddress_TMP_ = 0; // for the computed address

    private:

        // Next free address for consecutive allocation
        long long nextFreeAddress_ = 1;

        // -- Phase A: Map top-level procedures & main
        void mapProcedure(const std::shared_ptr<AST::Procedure> &proc);
        void mapMain(const std::shared_ptr<AST::Main> &mainPart);

        // Map local declarations
        void mapLocalDeclarations(const std::vector<std::shared_ptr<AST::Declarations>> &decls);

        // Assign an address to a single symbol
        void mapSymbol(const std::shared_ptr<AST::Symbol> &sym);

        // -- Phase B: Traverse commands, map loop variables
        void mapCommands(const std::vector<std::shared_ptr<AST::Command>> &commands);
        void mapCommand(const std::shared_ptr<AST::Command> &cmd);

    };

} // namespace Compiler
