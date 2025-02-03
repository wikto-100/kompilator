#pragma once

#include "ast.hpp"
#include "mem.hpp"

#include <memory>
#include <optional>
#include <string>
#include <vector>
#include <iostream>

namespace Compiler
{

    // ===================================================
    // Virtual Machine Instruction Set
    // ===================================================
    enum class VMInstruction
    {
        GET,
        PUT,
        LOAD,
        STORE,
        LOADI,
        STOREI,
        ADD,
        SUB,
        ADDI,
        SUBI,
        SET,
        HALF,
        JUMP,
        JPOS,
        JZERO,
        JNEG,
        RTRN,
        HALT
    };

    // ===================================================
    // Operand Enumeration for Binary Operations
    // ===================================================
    enum class OperandEnum
    {
        NUM_NUM,        // constant - constant
        NUM_SCALAR,     // constant - scalar
        SCALAR_NUM,     // scalar - constant
        SCALAR_SCALAR,  // scalar - scalar
        ARR_NUM,        // array - constant
        NUM_ARR,        // constant - array
        ARR_SCALAR,     // array - scalar
        SCALAR_ARR,     // scalar - array
        ARR_ARR,        // array - array
        NONE            // not applicable / unspecified
    };

    // ===================================================
    // Code Generation Instruction Structure
    // ===================================================
    struct CGInstruction
    {
        VMInstruction opcode;
        std::optional<long long> operand;
    };

    // ===================================================
    // CodeGenerator Class
    // ===================================================
    class CodeGenerator
    {
    public:
        CodeGenerator() = default;
        ~CodeGenerator() = default;

        // ------------------------------------
        // Top-level Code Generation Interface
        // ------------------------------------
        // Generates virtual machine code from the AST and writes the output to a file.
        void generate(const std::shared_ptr<AST::ProgramAll> &astRoot,
                      const std::string &outputFilename);

    private:
        // ===================================================
        // Optimization Functions
        // ===================================================
        void truncNoPUT(std::vector<CGInstruction> &m_code);
        // ===================================================
        // Emitter and Backpatcher Functions
        // ===================================================
        // Emit a VM instruction with no operand.
        long long emit(VMInstruction op);
        // Emit a VM instruction with an operand.
        long long emit(VMInstruction op, long long operand);
        // Backpatch the operand of an instruction at a given index.
        void backpatchOperand(long long instrIndex, long long newOperand);
        // Patch the jump instruction at jumpInstrIdx so that its offset points to the current code index.
        void patchJumpToCurrent(long long jumpInstrIdx);
        // Write the generated code to a file.
        void writeCodeToFile(const std::string &filename);

        // ===================================================
        // AST Traversal Functions
        // ===================================================
        // Generate code for the entire program.
        void genProgramAll(const AST::ProgramAll &prog);
        // Generate code for a procedure.
        void genProcedure(const AST::Procedure &proc);
        // Generate code for the main program.
        void genMain(const AST::Main &m);
        // Generate code for variable declarations.
        void genDeclarations(const AST::Declarations &decls);
        // Generate code for a list of commands.
        void genCommands(const std::vector<std::shared_ptr<AST::Command>> &cmds);
        // Generate code for a single command.
        void genCommand(const AST::Command &cmd);

        // ===================================================
        // Command-Specific Code Generation Functions
        // ===================================================
        void genAssign(const AST::AssignCmd &assign);
        void genIf(const AST::IfCmd &ifCmd);
        void genWhile(const AST::WhileCmd &whileCmd);
        void genRepeat(const AST::RepeatCmd &repeatCmd);
        void genFor(const AST::ForCmd &forCmd);
        void forHelper(const AST::ForCmd &forCmd, long long condStartIdx);

        void genProcCall(const AST::ProcCallCmd &callCmd);
        void genIO(const AST::IOCommand &ioCmd, AST::CommandType cmdType);

        // ===================================================
        // Expression and Condition Generation Functions
        // ===================================================
        void genExpression(const std::shared_ptr<AST::Expression> &expr);
        void genCondition(const std::shared_ptr<AST::Condition> &cond, VMInstruction &jumpType);

        // ===================================================
        // Medium-Granularity Helper Functions
        // ===================================================
        // Determine operand types for binary operations.
        void enumerateOperands(const AST::Value &lhs, const AST::Value &rhs, OperandEnum &opType);
        // Binary arithmetic operations.
        void genPlus(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        void genMinus(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        void genStar(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        void genSlash(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        
        long long python_floor_div(long long a, long long b, bool &isSafe);
        
        void genPercent(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        long long python_mod(long long a, long long b, bool &isSafe);
        // I/O Helpers: handle read and write operations.
        void handleReadTarget(const AST::Identifier &id);
        void handleWriteTarget(const AST::Identifier &id);

        // Array addressing helpers.
        void computeArrayElementAddr(const AST::Identifier &arrId, long long computedAddr);
        void loadArrayElement(const AST::Identifier &arrId);

        // Scalar memory operations.
        void loadScalar(const AST::Symbol &sym);
        void storeScalar(const AST::Symbol &sym);
        void subScalar(const AST::Symbol &sym);
        void addScalar(const AST::Symbol &sym);

    private:
        // Memory mapper for assigning addresses to variables and arrays.
        MemoryMapper mapper;
        // Container for generated code instructions.
        std::vector<CGInstruction> m_code;
    };

} // end namespace Compiler
