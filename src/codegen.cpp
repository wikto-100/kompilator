#include "codegen.hpp"
#include "safeops.hpp"
#include <fstream>
#include <iostream>
#include <algorithm>
namespace Compiler
{

    // ===================================================
    // Utility: Convert VMInstruction to string
    // ===================================================
    static const char *instrToString(VMInstruction instr)
    {
        switch (instr)
        {
        case VMInstruction::GET:
            return "GET";
        case VMInstruction::PUT:
            return "PUT";
        case VMInstruction::LOAD:
            return "LOAD";
        case VMInstruction::STORE:
            return "STORE";
        case VMInstruction::LOADI:
            return "LOADI";
        case VMInstruction::STOREI:
            return "STOREI";
        case VMInstruction::ADD:
            return "ADD";
        case VMInstruction::SUB:
            return "SUB";
        case VMInstruction::ADDI:
            return "ADDI";
        case VMInstruction::SUBI:
            return "SUBI";
        case VMInstruction::SET:
            return "SET";
        case VMInstruction::HALF:
            return "HALF";
        case VMInstruction::JUMP:
            return "JUMP";
        case VMInstruction::JPOS:
            return "JPOS";
        case VMInstruction::JZERO:
            return "JZERO";
        case VMInstruction::JNEG:
            return "JNEG";
        case VMInstruction::RTRN:
            return "RTRN";
        case VMInstruction::HALT:
            return "HALT";
        default:
            return "???";
        }
    }

    // ===================================================
    // Emitter and Backpatcher
    // ===================================================

    // Emit an instruction with no operand.
    long long CodeGenerator::emit(VMInstruction op)
    {
        CGInstruction instr{op, std::nullopt};
        m_code.push_back(instr);
        return (long long)m_code.size() - 1;
    }

    // Emit an instruction with an operand.
    long long CodeGenerator::emit(VMInstruction op, long long operand)
    {
        CGInstruction instr{op, operand};
        m_code.push_back(instr);
        return (long long)m_code.size() - 1;
    }

    // Backpatch the operand of an instruction at a given index.
    void CodeGenerator::backpatchOperand(long long instrIndex, long long newOperand)
    {
        if (instrIndex < 0 || instrIndex >= (long long)m_code.size())
        {
            std::cerr << "CodeGenerator: backpatch out of range.\n";
            return;
        }
        m_code[instrIndex].operand = newOperand;
    }

    // Helper: Patch the jump instruction at jumpInstrIdx so that its offset points to the current code index.
    void CodeGenerator::patchJumpToCurrent(long long jumpInstrIdx)
    {
        long long offset = (long long)m_code.size() - jumpInstrIdx;
        backpatchOperand(jumpInstrIdx, offset);
    }

    // Write the generated code to a file.
    void CodeGenerator::writeCodeToFile(const std::string &filename)
    {
        std::ofstream out(filename);
        if (!out.is_open())
        {
            std::cerr << "CodeGenerator: cannot open " << filename << "\n";
            return;
        }
        for (auto &instr : m_code)
        {
            if (instr.operand.has_value())
                out << instrToString(instr.opcode) << " " << instr.operand.value() << "\n";
            else
                out << instrToString(instr.opcode) << "\n";
        }
        out.close();
    }
    void CodeGenerator::truncNoPUT(std::vector<CGInstruction> &m_code)
    {
        // Check if any instruction in m_code is a PUT instruction.
        bool hasPut = std::any_of(m_code.begin(), m_code.end(), [](const CGInstruction &instr)
                                  { return instr.opcode == VMInstruction::PUT; });

        // If no PUT instruction is found, remove every instruction that is not GET.
        if (!hasPut)
        {
            m_code.erase(
                std::remove_if(m_code.begin(), m_code.end(), [](const CGInstruction &instr)
                               { return instr.opcode != VMInstruction::GET; }),
                m_code.end());
        }
    }
    // ===================================================
    // Public Interface: Code Generation Entry Point
    // ===================================================
    void CodeGenerator::generate(const std::shared_ptr<AST::ProgramAll> &astRoot,
                                 const std::string &outputFilename)
    {
        if (!astRoot)
        {
            std::cerr << "CodeGenerator: null AST.\n";
            return;
        }
        // Assign addresses for variables and arrays.
        mapper.assignAddresses(astRoot);

        // Generate code for the entire AST.
        genProgramAll(*astRoot);

        // Optimize the generated code by removing everything except GET instructions if no PUT is present.
        truncNoPUT(m_code);
        // Emit a final HALT instruction.
        emit(VMInstruction::HALT);
        // if no PUT in generated code, just output every GET from the generated code

        // Write the generated code to file.
        writeCodeToFile(outputFilename);
        std::cout << "CodeGenerator: wrote code to " << outputFilename << "\n";
    }

    // ===================================================
    // AST Traversal
    // ===================================================
    void CodeGenerator::genProgramAll(const AST::ProgramAll &prog)
    {
        // 1) Insert an unconditional jump at the very beginning
        //    that will skip all procedure code.
        long long skipToMainJump = 0;
        if (!prog.procedures.empty())
            skipToMainJump = emit(VMInstruction::JUMP, 0);

        // 2) Generate code for each procedure.
        //    They appear after this unconditional jump (so the main part will skip them).
        for (auto &p : prog.procedures)
        {
            genProcedure(*p);
        }

        // 3) Patch the skip-to-main jump so it lands right here,
        //    effectively skipping all the procedure code above.
        if (!prog.procedures.empty())
            patchJumpToCurrent(skipToMainJump);

        // 4) Finally, generate code for the main part if it exists.
        if (prog.mainPart)
        {
            genMain(*prog.mainPart);
        }
    }

    void CodeGenerator::genProcedure(const AST::Procedure &proc)
    {
        // 1) Record the procedure’s code start in proc.procSymbol->address
        //    So we know the position in m_code where the procedure begins.
        proc.procSymbol->address = static_cast<long long>(m_code.size());

        // 2) Now generate local declarations, commands, etc.
        for (auto &d : proc.localDeclarations)
        {
            genDeclarations(*d);
        }
        genCommands(proc.commands);

        // 3) Emit the final RTRN (this procedure returns to [returnAddrCell]).
        emit(VMInstruction::RTRN, proc.returnAddrCell);
    }

    void CodeGenerator::genMain(const AST::Main &m)
    {
        // Generate code for global declarations.
        for (auto &d : m.declarations)
        {
            genDeclarations(*d);
        }
        // Generate code for the main commands.
        genCommands(m.commands);
    }

    void CodeGenerator::genDeclarations(const AST::Declarations &decls)
    {
        // For each declared item, initialize offsets (used for arrays, etc.)
        for (auto &item : decls.items)
        {
            if (item.isArray && item.symbol)
            {
                long long base = item.symbol->address;
                long long start = item.rangeStart;
                long long offVal = base - start;
                long long offCell = base - 1;
                emit(VMInstruction::SET, offVal);
                emit(VMInstruction::STORE, offCell);
            }
        }
    }

    void CodeGenerator::genCommands(const std::vector<std::shared_ptr<AST::Command>> &cmds)
    {
        for (auto &c : cmds)
        {
            genCommand(*c);
        }
    }

    void CodeGenerator::genCommand(const AST::Command &cmd)
    {
        using CT = AST::CommandType;
        switch (cmd.type)
        {
        case CT::ASSIGN:
            if (cmd.assignCmd)
                genAssign(*cmd.assignCmd);
            break;
        case CT::IF_THEN:
        case CT::IF_THEN_ELSE:
            if (cmd.ifCmd)
                genIf(*cmd.ifCmd);
            break;
        case CT::WHILE:
            if (cmd.whileCmd)
                genWhile(*cmd.whileCmd);
            break;
        case CT::REPEAT:
            if (cmd.repeatCmd)
                genRepeat(*cmd.repeatCmd);
            break;
        case CT::FOR_TO:
        case CT::FOR_DOWNTO:
            if (cmd.forCmd)
                genFor(*cmd.forCmd);
            break;
        case CT::PROC_CALL:
            if (cmd.procCallCmd)
                genProcCall(*cmd.procCallCmd);
            break;
        case CT::READ:
        case CT::WRITE:
            if (cmd.ioCmd)
                genIO(*cmd.ioCmd, cmd.type);
            break;
        }
    }

    // ===================================================
    // Command-Specific Code Generation
    // ===================================================

    void CodeGenerator::genAssign(const AST::AssignCmd &assign)
    {
        if (!assign.lhs.symbol)
        {
            std::cerr << "genAssign: LHS has no symbol.\n";
            return;
        }
        // For array assignments, compute the effective address.
        if (assign.lhs.symbol->isArray)
        {
            computeArrayElementAddr(assign.lhs, mapper.computedAddress_RES_);
        }

        // Generate code to compute the RHS expression (result goes to mem[0]).
        if (assign.rhs)
        {
            genExpression(assign.rhs);
        }

        // Store the computed value into the target.
        if (assign.lhs.symbol->isArray)
        {
            emit(VMInstruction::STOREI, mapper.computedAddress_RES_);
        }
        else
        {
            storeScalar(*assign.lhs.symbol);
        }
    }

    void CodeGenerator::genIf(const AST::IfCmd &ifCmd)
    {
        VMInstruction jumpType = VMInstruction::JZERO;
        // Evaluate the condition; result is in the accumulator.
        genCondition(ifCmd.condition, jumpType);
        // Inversion flag: if the condition operator is EQ, GT, or LT, we swap the then/else blocks.
        bool inverted = ifCmd.condition->op == AST::CondOp::EQ ||
                        ifCmd.condition->op == AST::CondOp::GT ||
                        ifCmd.condition->op == AST::CondOp::LT;
        if (!inverted)
        {
            // --- Standard if-then-else ---
            // 1. Emit a conditional jump to skip the "then" block if condition fails.
            long long condJumpIdx = emit(jumpType, 0);
            // 2. Generate the "then" block.
            genCommands(ifCmd.thenCommands);
            // 3. Emit an unconditional jump to skip over the "else" block.
            long long jumpOverElseIdx = emit(VMInstruction::JUMP, 0);
            // 4. Patch the conditional jump to point to the beginning of the "else" block.
            patchJumpToCurrent(condJumpIdx);
            // 5. Generate the "else" block.
            genCommands(ifCmd.elseCommands);
            // 6. Patch the jump over else to point to the instruction after the if-statement.
            patchJumpToCurrent(jumpOverElseIdx);
        }
        else
        {
            // --- Inverted if-then-else (swapped then and else) ---
            long long condJumpIdx = emit(jumpType, 0);
            // Generate the "else" block first.
            genCommands(ifCmd.elseCommands);
            long long jumpOverThenIdx = emit(VMInstruction::JUMP, 0);
            // Patch the conditional jump to jump to the "then" block.
            patchJumpToCurrent(condJumpIdx);
            // Generate the "then" block.
            genCommands(ifCmd.thenCommands);
            // Patch the jump over then to continue after the if-statement.
            patchJumpToCurrent(jumpOverThenIdx);
        }
    }

    void CodeGenerator::genWhile(const AST::WhileCmd &whileCmd)
    {
        VMInstruction jumpType = VMInstruction::JZERO;
        bool inverted = whileCmd.condition->op == AST::CondOp::EQ ||
                        whileCmd.condition->op == AST::CondOp::GT ||
                        whileCmd.condition->op == AST::CondOp::LT;
        if (!inverted)
        {
            // --- Standard while-loop ---
            long long conditionStartIdx = (long long)m_code.size();
            genCondition(whileCmd.condition, jumpType);
            long long exitJumpIdx = emit(jumpType, 0);
            genCommands(whileCmd.body);
            // Emit jump back to re-evaluate the condition.
            long long jumpBackOffset = conditionStartIdx - (long long)m_code.size();
            emit(VMInstruction::JUMP, jumpBackOffset);
            // Patch the exit jump to point here (loop exit).
            patchJumpToCurrent(exitJumpIdx);
        }
        else
        {
            // --- Inverted while-loop ---
            long long conditionStartIdx = (long long)m_code.size();
            genCondition(whileCmd.condition, jumpType);
            // Emit a conditional jump that will skip over the exit jump.
            long long jumpOverExitIdx = emit(jumpType, 0);
            // Emit an unconditional exit jump (placeholder).
            long long exitJumpIdx = emit(VMInstruction::JUMP, 0);
            // Patch the conditional jump so it jumps to the start of the loop body.
            long long loopBodyStartIdx = (long long)m_code.size();
            backpatchOperand(jumpOverExitIdx, loopBodyStartIdx - jumpOverExitIdx);
            // Generate the loop body.
            genCommands(whileCmd.body);
            // Emit an unconditional jump to return to the condition.
            long long jumpBackOffset = conditionStartIdx - (long long)m_code.size();
            emit(VMInstruction::JUMP, jumpBackOffset);
            // Patch the exit jump to point to the code after the loop.
            patchJumpToCurrent(exitJumpIdx);
        }
    }

    void CodeGenerator::genRepeat(const AST::RepeatCmd &repeatCmd)
    {
        // Record the start index of the loop.
        long long loopStartIdx = (long long)m_code.size();

        // Execute the loop body (repeat‑until guarantees at least one execution).
        genCommands(repeatCmd.body);

        // Evaluate the loop condition.
        // The generated condition places its result in the accumulator.
        // The jumpType (e.g., JZERO, JPOS, or JNEG) is chosen by genCondition.
        VMInstruction jumpType = VMInstruction::JZERO;
        genCondition(repeatCmd.condition, jumpType);

        // Determine whether the condition requires inverted handling.
        // (For example, if the operator is EQ, GT, or LT, we use a different jump arrangement.)
        bool inverted = (repeatCmd.condition->op == AST::CondOp::EQ ||
                         repeatCmd.condition->op == AST::CondOp::GT ||
                         repeatCmd.condition->op == AST::CondOp::LT);

        if (!inverted)
        {
            // ----------------------------------------------------------------
            // Non-Inverted Repeat-Until:
            // Structure:
            //   [loop body]
            //   [condition evaluation]
            //   JCOND ?   // if condition fails (i.e., false) then jump back to loop start
            // If the condition is false, execution jumps back to the beginning.
            // ----------------------------------------------------------------
            long long jumpCondIdx = emit(jumpType, 0);

            // Compute the relative offset to jump back to the start of the loop.
            long long jumpOffset = loopStartIdx - (long long)m_code.size();
            backpatchOperand(jumpCondIdx, jumpOffset);
        }
        else
        {
            // ----------------------------------------------------------------
            // Inverted Repeat-Until:
            // Structure:
            //   [loop body]
            //   [condition evaluation]
            //   JCOND ?   // if condition is met in the inverted sense, skip the jump back (exit loop)
            //   JUMP ?    // unconditional jump back to loop start
            // ----------------------------------------------------------------
            // Emit the conditional jump that, when taken, skips over the jump-back.
            long long jumpSkipLoopIdx = emit(jumpType, 0);
            // Emit an unconditional jump that will jump back to the start of the loop.
            long long jumpBackIdx = emit(VMInstruction::JUMP, 0);

            // Patch the conditional jump so that, if the condition is met (inverted sense),
            // it skips over the jump-back.
            long long afterJumpIdx = (long long)m_code.size();
            backpatchOperand(jumpSkipLoopIdx, afterJumpIdx - jumpSkipLoopIdx);

            // Patch the unconditional jump to point back to the beginning of the loop.
            long long jumpBackOffset = loopStartIdx - ((long long)m_code.size() - 1);
            backpatchOperand(jumpBackIdx, jumpBackOffset);
        }
    }

    void CodeGenerator::genFor(const AST::ForCmd &forCmd)
    {
        using OE = OperandEnum;
        OperandEnum seType = OperandEnum::NONE;
        enumerateOperands(*forCmd.startValue, *forCmd.endValue, seType);

        switch (seType)
        {
        case OE::NUM_NUM:
        {
            // Both start and end are constant numbers.
            long long start = forCmd.startValue->numberValue;
            long long end = forCmd.endValue->numberValue;

            // For a downward loop, we require start >= end.
            // For an upward loop, we require start <= end.
            if (forCmd.downTo)
            {
                if (start < end)
                    return; // Loop would never execute.
            }
            else
            {
                if (start > end)
                    return; // Loop would never execute.
            }

            // Initialize the loop variable with the start value.
            emit(VMInstruction::SET, start);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);

            // --- Condition Check ---
            // Record the index where the condition check begins.
            long long condStartIdx = (long long)m_code.size();
            // Compute (end - loopVar) for the exit test.
            // (The same arithmetic is used for both upward and downward loops.)
            emit(VMInstruction::SET, end);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            // Call the generic helper which handles:
            // - Emitting the conditional exit jump (JPOS for downto, JNEG for upto),
            // - Generating the loop body,
            // - Updating the loop variable (adding -1 for downto, +1 for upto),
            // - Emitting an unconditional jump back,
            // - Patching the exit jump.
            forHelper(forCmd, condStartIdx);
            break;
        }
        case OE::NUM_SCALAR:
        {
            // Start is constant and end is a scalar.
            long long start = forCmd.startValue->numberValue;
            // Initialize the loop variable with the start value.
            emit(VMInstruction::SET, start);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);
            // Load the end value into the accumulator.
            long long condStartIdx = (long long)m_code.size();
            loadScalar(*forCmd.endValue->identifier.symbol);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);

            break;
        }
        case OE::SCALAR_NUM:
        {
            // End is constant and start is a scalar.
            long long end = forCmd.endValue->numberValue;
            loadScalar(*forCmd.startValue->identifier.symbol);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);
            long long condStartIdx = (long long)m_code.size();
            emit(VMInstruction::SET, end);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);
            break;
        }
        case OE::SCALAR_SCALAR:
        {
            // Both start and end are scalar variables.
            loadScalar(*forCmd.startValue->identifier.symbol);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);
            long long condStartIdx = (long long)m_code.size();
            loadScalar(*forCmd.endValue->identifier.symbol);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);
            break;
        }
        case OE::ARR_NUM:
        {
            // Start is an array element and end is a constant.
            computeArrayElementAddr(forCmd.startValue->identifier, mapper.computedAddress_TMP_);
            emit(VMInstruction::LOADI, mapper.computedAddress_TMP_);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);
            long long condStartIdx = (long long)m_code.size();
            emit(VMInstruction::SET, forCmd.endValue->numberValue);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);
            break;
        }
        case OE::NUM_ARR:
        {
            // Start is a constant and end is an array element.
            long long start = forCmd.startValue->numberValue;
            computeArrayElementAddr(forCmd.endValue->identifier, mapper.computedAddress_TMP_);
            // Initialize the loop variable with the start value.
            emit(VMInstruction::SET, start);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);
            long long condStartIdx = (long long)m_code.size();
            // Load the end value into the accumulator.
            emit(VMInstruction::LOADI, mapper.computedAddress_TMP_);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);
            break;
        }
        case OE::ARR_SCALAR:
        {
            // Start is an array element and end is a scalar.
            computeArrayElementAddr(forCmd.startValue->identifier, mapper.computedAddress_TMP_);
            emit(VMInstruction::LOADI, mapper.computedAddress_TMP_);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);
            long long condStartIdx = (long long)m_code.size();
            loadScalar(*forCmd.endValue->identifier.symbol);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);
            break;
        }
        case OE::SCALAR_ARR:
        {
            // Start is a scalar and end is an array element.
            computeArrayElementAddr(forCmd.endValue->identifier, mapper.computedAddress_TMP_);
            loadScalar(*forCmd.startValue->identifier.symbol);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);
            long long condStartIdx = (long long)m_code.size();
            emit(VMInstruction::LOADI, mapper.computedAddress_TMP_);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);
            break;
        }
        case OE::ARR_ARR:
        {
            computeArrayElementAddr(forCmd.startValue->identifier, mapper.computedAddress_TMP_);
            computeArrayElementAddr(forCmd.endValue->identifier, mapper.T1_); // careful of trashing T1_ (may be used elsewhere)
            emit(VMInstruction::LOADI, mapper.computedAddress_TMP_);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);
            long long condStartIdx = (long long)m_code.size();
            emit(VMInstruction::LOADI, mapper.T1_);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);
            break;
        }
        default:
            break;
        }
    }

    void CodeGenerator::forHelper(const AST::ForCmd &forCmd, long long condStartIdx)
    {
        // Determine the update step and exit opcode based on the loop direction.
        // For a downward (downto) loop:
        //    step = -1 and we exit when (end - loopVar) > 0 (use JPOS).
        // For an upward (upto) loop:
        //    step = +1 and we exit when (end - loopVar) < 0 (use JNEG).
        VMInstruction exitOpcode = forCmd.downTo ? VMInstruction::JPOS : VMInstruction::JNEG;
        long long jumpExitIdx = emit(exitOpcode, 0);

        // --- Loop Body ---
        // Generate the code for the loop body.
        genCommands(forCmd.body);

        // --- Update Step ---
        long long step = forCmd.downTo ? -1 : 1;
        emit(VMInstruction::SET, step);
        emit(VMInstruction::ADD, forCmd.loopVarSymbol->address);
        emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);

        // --- Unconditional Jump Back ---
        long long jumpBackOffset = condStartIdx - (long long)m_code.size();
        emit(VMInstruction::JUMP, jumpBackOffset);

        // --- Loop Exit ---
        // Patch the conditional exit jump to point to the instruction immediately following the loop.
        patchJumpToCurrent(jumpExitIdx);
    }

    void CodeGenerator::genProcCall(const AST::ProcCallCmd &callCmd)
    {
        // 1) Basic sanity checks
        if (!callCmd.callee || !callCmd.callee->procSymbol)
        {
            std::cerr << "genProcCall: error - no callee or missing callee->procSymbol\n";
            emit(VMInstruction::HALT);
            return;
        }

        auto calleeSymbol = callCmd.callee->procSymbol;

        // 2) Check argument count vs. parameter count (already done in semantic analysis)
        // 3) Pointer setup
        //    For each parameter, store either array offset or scalar address
        //    from argSymbols[i] into paramSymbols[i].
        for (size_t i = 0; i < callCmd.argSymbols.size(); i++)
        {
            auto argSym = callCmd.argSymbols[i];             // Symbol for the caller’s argument
            auto paramSym = callCmd.callee->paramSymbols[i]; // Symbol for the callee’s parameter cell

            if (!argSym || !paramSym)
            {
                std::cerr << "genProcCall: null symbol for argument or parameter\n";
                emit(VMInstruction::HALT);
                return;
            }

            // If the argument is an array, store the offsetCell pointer;
            // otherwise, store the scalar’s single cell address.
            // what if the argument is a reference parameter?
            if (argSym->isArray)
            {
                // oh boy, what if the array we are passing is a reference parameter?
                // we need to load something that is storing the offset cell of the array
                // cause normally we just have the offset cell of the array
                if (!argSym->isRefParam)
                    emit(VMInstruction::SET, argSym->offsetCell);
                else
                {
                    emit(VMInstruction::LOAD, argSym->address);
                }

                emit(VMInstruction::STORE, paramSym->address);
            }
            else
            {
                if (!argSym->isRefParam)
                    emit(VMInstruction::SET, argSym->address);
                else
                    emit(VMInstruction::LOAD, argSym->address);

                emit(VMInstruction::STORE, paramSym->address);
            }
        }

        // 4) Store the return address (for a non‐recursive language)
        long long retIP = (long long)m_code.size() + 3; // offset depends on your VM's jump mechanics
        long long retCell = callCmd.callee->returnAddrCell;
        emit(VMInstruction::SET, retIP);
        emit(VMInstruction::STORE, retCell);

        // 5) Jump to the procedure’s code
        //    The callee eventually does an RTRN, which loads IP from [returnAddrCell].
        long long currentIndex = (long long)m_code.size();

        // The callee's code start is stored in callee->procSymbol->address.
        long long procedureStartIndex = callCmd.callee->procSymbol->address;

        // Since your VM does *not* advance IP after decoding a jump,
        // the jump offset is:
        //   offset = procedureStartIndex - currentIndex
        long long offset = procedureStartIndex - currentIndex;

        // Now emit the jump with this offset.
        emit(VMInstruction::JUMP, offset);
    }

    void CodeGenerator::genIO(const AST::IOCommand &ioCmd, AST::CommandType cmdType)
    {
        if (cmdType == AST::CommandType::READ)
        {
            if (!ioCmd.readTarget)
                return;
            auto sym = ioCmd.readTarget->symbol;
            if (!sym)
            {
                std::cerr << "genIO: readTarget has no symbol.\n";
                return;
            }
            handleReadTarget(*ioCmd.readTarget);
        }
        else // WRITE
        {
            if (!ioCmd.writeValue.has_value())
                return;
            auto val = ioCmd.writeValue.value();
            if (val->isIdentifier)
            {
                handleWriteTarget(val->identifier);
            }
            else
            {
                long long num = val->numberValue;
                emit(VMInstruction::SET, num);
                emit(VMInstruction::PUT, 0);
            }
        }
    }

    void CodeGenerator::handleReadTarget(const AST::Identifier &id)
    {
        auto sym = id.symbol;
        if (!sym)
        {
            std::cerr << "handleReadTarget: no symbol.\n";
            return;
        }
        if (sym->isArray)
        {
            computeArrayElementAddr(id, mapper.computedAddress_TMP_);
            emit(VMInstruction::GET, 0);
            emit(VMInstruction::STOREI, mapper.computedAddress_TMP_);
        }
        else
        {
            if (sym->isRefParam)
            {
                emit(VMInstruction::GET, 0);
                emit(VMInstruction::STOREI, sym->address);
            }
            else
            {
                emit(VMInstruction::GET, sym->address);
            }
        }
    }

    void CodeGenerator::handleWriteTarget(const AST::Identifier &id)
    {
        auto sym = id.symbol;
        if (!sym)
        {
            std::cerr << "handleWriteTarget: no symbol.\n";
            return;
        }
        if (sym->isArray)
        {
            loadArrayElement(id);
            emit(VMInstruction::PUT, 0);
        }
        else
        {
            if (sym->isRefParam)
            {
                emit(VMInstruction::LOADI, sym->address);
                emit(VMInstruction::PUT, 0);
            }
            else
            {
                emit(VMInstruction::PUT, sym->address);
            }
        }
    }

    // ===================================================
    // Expression and Condition Generation
    // ===================================================

    void CodeGenerator::genExpression(const std::shared_ptr<AST::Expression> &expr)
    {
        if (!expr)
            return;

        // Simple value (literal or identifier)
        if (expr->op == AST::ExprOp::NONE)
        {
            if (!expr->left)
            {
                std::cerr << "genExpression: expr->left is null\n";
                return;
            }
            if (expr->left->isIdentifier)
            {
                if (auto sym = expr->left->identifier.symbol)
                {
                    if (!sym->isArray)
                        loadScalar(*sym);
                    else
                        loadArrayElement(expr->left->identifier);
                }
                else
                {
                    std::cerr << "genExpression: left identifier has no symbol\n";
                }
            }
            else
            {
                // Numeric literal
                emit(VMInstruction::SET, expr->left->numberValue);
            }
            return;
        }

        // Binary expression: determine operand types and generate corresponding code.
        OperandEnum opType = OperandEnum::NONE;
        enumerateOperands(*expr->left, *expr->right, opType);

        switch (expr->op)
        {
        case AST::ExprOp::PLUS:
            genPlus(*expr->left, *expr->right, opType);
            break;
        case AST::ExprOp::MINUS:
            genMinus(*expr->left, *expr->right, opType);
            break;
        case AST::ExprOp::STAR:
            genStar(*expr->left, *expr->right, opType);
            break;
        case AST::ExprOp::SLASH:
            genSlash(*expr->left, *expr->right, opType);
            break;
        case AST::ExprOp::PERCENT:
            genPercent(*expr->left, *expr->right, opType);
            break;
        default:
            std::cerr << "genExpression: unknown op\n";
            break;
        }
    }

    void CodeGenerator::genCondition(const std::shared_ptr<AST::Condition> &cond, VMInstruction &jumpType)
    {
        if (!cond || !cond->left || !cond->right)
        {
            std::cerr << "genCondition: missing left/right\n";
            return;
        }
        OperandEnum opType = OperandEnum::NONE;
        enumerateOperands(*cond->left, *cond->right, opType);
        switch (cond->op)
        {
        case AST::CondOp::EQ:
            genMinus(*cond->left, *cond->right, opType);
            jumpType = VMInstruction::JZERO;
            break;
        case AST::CondOp::NEQ:
            genMinus(*cond->left, *cond->right, opType);
            jumpType = VMInstruction::JZERO;
            break;
        case AST::CondOp::GT:
            genMinus(*cond->left, *cond->right, opType);
            jumpType = VMInstruction::JPOS;
            break;
        case AST::CondOp::LT:
            genMinus(*cond->left, *cond->right, opType);
            jumpType = VMInstruction::JNEG;
            break;
        case AST::CondOp::GEQ:
            genMinus(*cond->left, *cond->right, opType);
            jumpType = VMInstruction::JNEG;
            break;
        case AST::CondOp::LEQ:
            genMinus(*cond->left, *cond->right, opType);
            jumpType = VMInstruction::JPOS;
            break;
        default:
            std::cerr << "genCondition: unknown op\n";
            break;
        }
    }

    void CodeGenerator::enumerateOperands(const AST::Value &lhs, const AST::Value &rhs, OperandEnum &opType)
    {
        // Both operands are numeric literals.
        if (!lhs.isIdentifier && !rhs.isIdentifier)
        {
            opType = OperandEnum::NUM_NUM;
            return;
        }
        // Left is identifier, right is literal.
        if (lhs.isIdentifier && !rhs.isIdentifier)
        {
            auto symL = lhs.identifier.symbol;
            opType = (symL->isArray ? OperandEnum::ARR_NUM : OperandEnum::SCALAR_NUM);
            return;
        }
        // Left is literal, right is identifier.
        if (!lhs.isIdentifier && rhs.isIdentifier)
        {
            auto symR = rhs.identifier.symbol;
            opType = (symR->isArray ? OperandEnum::NUM_ARR : OperandEnum::NUM_SCALAR);
            return;
        }
        // Both operands are identifiers.
        if (lhs.isIdentifier && rhs.isIdentifier)
        {
            auto symL = lhs.identifier.symbol;
            auto symR = rhs.identifier.symbol;
            if (symL->isArray && symR->isArray)
                opType = OperandEnum::ARR_ARR;
            else if (symL->isArray && !symR->isArray)
                opType = OperandEnum::ARR_SCALAR;
            else if (!symL->isArray && symR->isArray)
                opType = OperandEnum::SCALAR_ARR;
            else if (!symL->isArray && !symR->isArray)
                opType = OperandEnum::SCALAR_SCALAR;
            return;
        }
    }

    // ===================================================
    // Medium-Granularity Helpers: Binary Ops, Array Addressing, and Scalar Operations
    // ===================================================

    // Binary addition
    void CodeGenerator::genPlus(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType)
    {
        using OE = OperandEnum;
        switch (opType)
        {
        case OE::NUM_NUM:
        {
            long long result;
            bool isSafeFold = safeops::safeAdd(lhs.numberValue, rhs.numberValue, result);
            if (isSafeFold)
                emit(VMInstruction::SET, result);
            else
            {
                emit(VMInstruction::SET, lhs.numberValue);
                emit(VMInstruction::STORE, mapper.T1_);
                emit(VMInstruction::SET, rhs.numberValue);
                emit(VMInstruction::ADD, mapper.T1_);
            }
            break;
        }
        case OE::NUM_SCALAR:
        {
            long long lNum = lhs.numberValue;
            if (lNum == 0)
            {
                loadScalar(*rhs.identifier.symbol);
                break;
            }
            emit(VMInstruction::SET, lNum);
            addScalar(*rhs.identifier.symbol);
            break;
        }
        case OE::NUM_ARR:
        {
            long long lNum = lhs.numberValue;
            if (lNum == 0)
            {
                loadArrayElement(rhs.identifier);
                break;
            }
            computeArrayElementAddr(rhs.identifier, mapper.computedAddress_TMP_);
            emit(VMInstruction::SET, lNum);
            emit(VMInstruction::ADDI, mapper.computedAddress_TMP_);
            break;
        }
        case OE::SCALAR_NUM:
        {
            long long rNum = rhs.numberValue;
            if (rNum == 0)
            {
                loadScalar(*lhs.identifier.symbol);
                break;
            }
            emit(VMInstruction::SET, rNum);
            addScalar(*lhs.identifier.symbol);
            break;
        }
        case OE::SCALAR_SCALAR:
        {
            loadScalar(*lhs.identifier.symbol);
            addScalar(*rhs.identifier.symbol);
            break;
        }
        case OE::SCALAR_ARR:
        {
            loadArrayElement(rhs.identifier);
            addScalar(*lhs.identifier.symbol);
            break;
        }
        case OE::ARR_NUM:
        {
            long long rNum = rhs.numberValue;
            if (rNum == 0)
            {
                loadArrayElement(lhs.identifier);
                break;
            }
            computeArrayElementAddr(lhs.identifier, mapper.computedAddress_TMP_);
            emit(VMInstruction::SET, rNum);
            emit(VMInstruction::ADDI, mapper.computedAddress_TMP_);
            break;
        }
        case OE::ARR_SCALAR:
        {
            loadArrayElement(lhs.identifier);
            addScalar(*rhs.identifier.symbol);
            break;
        }
        case OE::ARR_ARR:
        {
            loadArrayElement(lhs.identifier);
            emit(VMInstruction::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMInstruction::ADD, mapper.T1_);
            break;
        }
        default:
            break;
        }
    }

    // Binary subtraction
    void CodeGenerator::genMinus(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType)
    {
        using OE = OperandEnum;
        switch (opType)
        {
        case OE::NUM_NUM:
        {
            long long result;
            bool isSafeFold = safeops::safeSubtract(lhs.numberValue, rhs.numberValue, result);
            if (isSafeFold)
                emit(VMInstruction::SET, result);
            else
            {
                emit(VMInstruction::SET, rhs.numberValue);
                emit(VMInstruction::STORE, mapper.T1_);
                emit(VMInstruction::SET, lhs.numberValue);
                emit(VMInstruction::SUB, mapper.T1_);
            }
            break;
        }
        case OE::NUM_SCALAR:
        {
            long long lNum = lhs.numberValue;
            emit(VMInstruction::SET, lNum);
            subScalar(*rhs.identifier.symbol);
            break;
        }
        case OE::NUM_ARR:
        {
            computeArrayElementAddr(rhs.identifier, mapper.computedAddress_TMP_);
            emit(VMInstruction::SET, lhs.numberValue);
            emit(VMInstruction::SUBI, mapper.computedAddress_TMP_);
            break;
        }
        case OE::SCALAR_NUM:
        {
            long long rNumNeg = -rhs.numberValue;

            // if number is zero, then just load the scalar
            if (rNumNeg == 0)
            {
                loadScalar(*lhs.identifier.symbol);
                break;
            }
            emit(VMInstruction::SET, rNumNeg);
            addScalar(*lhs.identifier.symbol);
            break;
        }
        case OE::SCALAR_SCALAR:
        {

            loadScalar(*lhs.identifier.symbol);
            subScalar(*rhs.identifier.symbol);
            break;
        }
        case OE::SCALAR_ARR:
        {
            computeArrayElementAddr(rhs.identifier, mapper.computedAddress_TMP_);
            loadScalar(*lhs.identifier.symbol);
            emit(VMInstruction::SUBI, mapper.computedAddress_TMP_);
            break;
        }
        case OE::ARR_NUM:
        {
            long long rNumNeg = -rhs.numberValue;
            if (rNumNeg == 0)
            {
                loadArrayElement(lhs.identifier);
                break;
            }
            computeArrayElementAddr(lhs.identifier, mapper.computedAddress_TMP_);
            emit(VMInstruction::SET, rNumNeg);
            emit(VMInstruction::ADDI, mapper.computedAddress_TMP_);
            break;
        }
        case OE::ARR_SCALAR:
        {
            loadArrayElement(lhs.identifier);
            subScalar(*rhs.identifier.symbol);
            break;
        }
        case OE::ARR_ARR:
        {
            loadArrayElement(rhs.identifier);
            emit(VMInstruction::STORE, mapper.T1_);
            loadArrayElement(lhs.identifier);
            emit(VMInstruction::SUB, mapper.T1_);
            break;
        }
        default:
            break;
        }
    }

    // Binary multiplication
    void CodeGenerator::genStar(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType)
    {
        using OE = OperandEnum;
        using VMI = VMInstruction;

        // Helper lambda to emit sign-flip: result = -accumulator
        auto emitSignFlip = [&]()
        {
            // current accumulator has someValue
            // we want 0 - someValue
            // Easiest approach: store it temporarily, SET 0, SUB the temp
            emit(VMI::STORE, mapper.T3_); // T3_ ← someValue
            emit(VMI::SET, 0);            // p[0] ← 0
            emit(VMI::SUB, mapper.T3_);   // p[0] ← 0 - T3_ = -someValue
        };

        // ---------- 1) SWITCH over operand type -----------
        switch (opType)
        {
        case OE::NUM_NUM:
        {
            // Both operands are known constants => do compile-time fold
            long long result;
            bool isSafeFold = safeops::safeMultiply(lhs.numberValue, rhs.numberValue, result);
            if (isSafeFold)
            {
                emit(VMI::SET, result);
                return;
            }
            else
            {
                emit(VMI::SET, lhs.numberValue);
                emit(VMI::STORE, mapper.T1_);
                emit(VMI::SET, rhs.numberValue);
                emit(VMI::STORE, mapper.T2_);
                break;
            }
        }

        // ============= CASES WHERE LHS IS CONSTANT =================
        case OE::NUM_SCALAR:
        {
            const long long cst = lhs.numberValue;

            // If cst in {-2, -1, 0, 1, 2}, short-circuit
            switch (cst)
            {
            case 0:
                // 0 * (scalar) => 0
                emit(VMI::SET, 0);
                return;
            case 1:
                // 1 * (scalar) => load the scalar
                loadScalar(*rhs.identifier.symbol);
                return;
            case -1:
                // -1 * (scalar) => sign flip of the scalar
                loadScalar(*rhs.identifier.symbol); // accumulator <- scalar
                emitSignFlip();
                return;
            case 2:
                // 2 * (scalar) => scalar + scalar
                loadScalar(*rhs.identifier.symbol); // p[0] = scalar
                emit(VMI::ADD, 0);                  // p[0] = scalar + scalar    (2*scalar)
                return;
            case -2:
            {
                // -2 * (scalar) => -(scalar + scalar)
                loadScalar(*rhs.identifier.symbol);
                emit(VMI::ADD, 0);
                emitSignFlip(); // p[0] <- - (2*scalar)
                return;
            }
            default:
                // Not a small constant => proceed with normal code
                break;
            }

            // If we get here, cst is not in {-2,-1,0,1,2}
            // Then do normal approach: load cst => T1_, load scalar => T2_
            emit(VMI::SET, cst);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }

        case OE::NUM_ARR:
        {
            const long long cst = lhs.numberValue;
            // same structure:
            switch (cst)
            {
            case 0:
                emit(VMI::SET, 0);
                return;
            case 1:
                // just load the array element
                loadArrayElement(rhs.identifier);
                return;
            case -1:
                loadArrayElement(rhs.identifier);
                emitSignFlip();
                return;
            case 2:
            {
                // 2 * arrayElement => read it, double it
                loadArrayElement(rhs.identifier);
                emit(VMI::ADD, 0);
                return;
            }
            case -2:
            {
                // -2 * arrayElement
                loadArrayElement(rhs.identifier);
                emit(VMI::ADD, 0);
                emitSignFlip();
                return;
            }
            default:
                break;
            }

            // not a small constant => normal approach
            emit(VMI::SET, cst);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }

        // ============= CASES WHERE RHS IS CONSTANT =================
        case OE::SCALAR_NUM:
        {
            const long long cst = rhs.numberValue;
            switch (cst)
            {
            case 0:
                // scalar * 0 => 0
                emit(VMI::SET, 0);
                return;
            case 1:
                // scalar * 1 => just load scalar
                loadScalar(*lhs.identifier.symbol);
                return;
            case -1:
                loadScalar(*lhs.identifier.symbol);
                emitSignFlip();
                return;
            case 2:
            {
                // scalar * 2 => scalar + scalar
                loadScalar(*lhs.identifier.symbol);
                emit(VMI::ADD, 0);
                return;
            }
            case -2:
            {
                loadScalar(*lhs.identifier.symbol);
                emit(VMI::ADD, 0);
                emitSignFlip();
                return;
            }
            default:
                break;
            }
            // normal approach
            loadScalar(*lhs.identifier.symbol);
            emit(VMI::STORE, mapper.T1_);
            emit(VMI::SET, cst);
            emit(VMI::STORE, mapper.T2_);
            break;
        }

        case OE::ARR_NUM:
        {
            const long long cst = rhs.numberValue;
            switch (cst)
            {
            case 0:
                emit(VMI::SET, 0);
                return;
            case 1:
                loadArrayElement(lhs.identifier);
                return;
            case -1:
                loadArrayElement(lhs.identifier);
                emitSignFlip();
                return;
            case 2:
            {
                loadArrayElement(lhs.identifier);
                emit(VMI::ADD, 0);
                return;
            }
            case -2:
            {
                loadArrayElement(lhs.identifier);
                emit(VMI::ADD, 0);
                emitSignFlip();
                return;
            }
            default:
                break;
            }
            // normal approach
            loadArrayElement(lhs.identifier);
            emit(VMI::STORE, mapper.T1_);
            emit(VMI::SET, cst);
            emit(VMI::STORE, mapper.T2_);
            break;
        }

        // ============= CASES WHERE NEITHER IS CONSTANT =============
        case OE::SCALAR_SCALAR:
        {
            // no short-circuit here
            loadScalar(*lhs.identifier.symbol);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::SCALAR_ARR:
        {
            loadScalar(*lhs.identifier.symbol);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::ARR_SCALAR:
        {
            loadArrayElement(lhs.identifier);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::ARR_ARR:
        {
            loadArrayElement(lhs.identifier);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        default:
            // Should not happen
            return;
        }

        // ============== 2) EMIT THE FULL MULTIPLICATION =============
        // If we get here, neither operand was a "shortcut constant"
        // or at least we have already loaded T1_ and T2_ in the appropriate cells

        // The standard "Russian–peasant" multiplication code
        // We'll rely on T1_ (multiplicand), T2_ (multiplier), T3_ (partial sum), T4_ (temp)

        // Initialize partial sum T3_ to 0
        emit(VMI::LOAD, mapper.T1_);  // p[0] = T1_
        emit(VMI::SUB, mapper.T1_);   // p[0] = T1_ - T1_ = 0
        emit(VMI::STORE, mapper.T3_); // T3_ = 0

        // Sign fix: if T2_ < 0, then flip T1_ and T2_
        emit(VMI::LOAD, mapper.T2_);
        emit(VMI::JNEG, 2); // jump +2 if negative
        emit(VMI::JUMP, 7); // else skip sign-fix
        // sign-fix block:
        emit(VMI::LOAD, mapper.T3_); // T3_ is 0
        emit(VMI::SUB, mapper.T1_);  // => -T1_
        emit(VMI::STORE, mapper.T1_);
        emit(VMI::LOAD, mapper.T3_); // still 0
        emit(VMI::SUB, mapper.T2_);  // => -T2_
        emit(VMI::STORE, mapper.T2_);
        // main loop:
        emit(VMI::LOAD, mapper.T2_);
        emit(VMI::JZERO, 15);         // if T2_ == 0 => jump +15 to the end
        emit(VMI::STORE, mapper.T4_); // T4_ = T2_
        emit(VMI::HALF);              // p[0] = floor(T2_/2)
        emit(VMI::STORE, mapper.T2_); // T2_ = floor(T2_/2)
        emit(VMI::LOAD, mapper.T4_);  // p[0] = original T2_
        emit(VMI::SUB, mapper.T2_);   // p[0] = T2_ - floor(T2_/2)
        emit(VMI::SUB, mapper.T2_);   // p[0] = T2_ - 2*floor(T2_/2) => 0 if even, else 1 or -1
        emit(VMI::JZERO, 4);          // if even => skip partialSum += T1_
        emit(VMI::LOAD, mapper.T3_);  // partialSum
        emit(VMI::ADD, mapper.T1_);   // partialSum += T1_
        emit(VMI::STORE, mapper.T3_);
        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::ADD, mapper.T1_); // T1_ *= 2
        emit(VMI::STORE, mapper.T1_);
        emit(VMI::JUMP, -15); // jump back 15 lines to re-check T2_
        // end of loop => load partial sum into accumulator
        emit(VMI::LOAD, mapper.T3_); // final product in p[0]
    }
    long long CodeGenerator::python_floor_div(long long a, long long b, bool &isSafe)
    {
        if (b == 0 || a == 0)
        {
            return 0;
        }

        // Compute the truncated quotient.
        long long quotient;
        isSafe = safeops::safeDivide(a, b, quotient);
        if (quotient == LLONG_MIN)
        {
            isSafe = false;
            return 0;
        }

        long long remainder = a % b;

        // If there is a nonzero remainder and a and b have opposite signs,
        // subtract one from the quotient to achieve floor division.
        if (remainder != 0 && ((a < 0) != (b < 0)))
        {
            quotient -= 1;
        }

        return quotient;
    }
    // Binary division
    void CodeGenerator::genSlash(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType)
    {

        using OE = OperandEnum;
        using VMI = VMInstruction;

        // Case: Both operands are numeric constants.
        if (opType == OE::NUM_NUM)
        {
            bool isSafe = false;
            long long result = python_floor_div(lhs.numberValue, rhs.numberValue, isSafe);
            if (isSafe)
            {
                emit(VMI::SET, result);
                return;
            }
            else
            {
                emit(VMI::SET, lhs.numberValue);
                emit(VMI::STORE, mapper.T1_);
                emit(VMI::SET, rhs.numberValue);
                emit(VMI::STORE, mapper.T2_);
            }
        }

        // ----- Constant Folding for cases when RHS is a constant (SCALAR_NUM or ARR_NUM) -----
        bool rhsIsConst = (opType == OE::SCALAR_NUM || opType == OE::ARR_NUM);
        if (rhsIsConst)
        {
            long long cstRHS = rhs.numberValue;
            switch (cstRHS)
            {
            case 0:
                // Division by 0 => result 0.
                emit(VMI::SET, 0);
                return;
            case 1:
                // Division by 1: result = LHS.
                if (opType == OE::SCALAR_NUM)
                {
                    loadScalar(*lhs.identifier.symbol);
                }
                else
                { // OE::ARR_NUM
                    loadArrayElement(lhs.identifier);
                }
                return;
            case -1:
                // Division by -1: result = -LHS.
                if (opType == OE::SCALAR_NUM)
                {
                    loadScalar(*lhs.identifier.symbol);
                }
                else
                {
                    loadArrayElement(lhs.identifier);
                }
                {
                    // Flip sign.
                    emit(VMI::STORE, mapper.T3_); // store current value in T3_
                    emit(VMI::SET, 0);
                    emit(VMI::SUB, mapper.T3_);
                }
                return;
            case 2:
                // Division by 2: result = floor(LHS/2).
                if (opType == OE::SCALAR_NUM)
                {
                    loadScalar(*lhs.identifier.symbol);
                }
                else
                {
                    loadArrayElement(lhs.identifier);
                }
                emit(VMI::HALF);
                return;
            case -2:
                // Division by -2: result = floor(LHS/(-2)).
                if (opType == OE::SCALAR_NUM)
                {
                    loadScalar(*lhs.identifier.symbol);
                }
                else
                {
                    loadArrayElement(lhs.identifier);
                }
                emit(VMI::HALF);
                {
                    // Flip sign.
                    emit(VMI::STORE, mapper.T3_);
                    emit(VMI::SET, 0);
                    emit(VMI::SUB, mapper.T3_);
                }
                return;
            default:
                // Not a special case constant; fall through to general code.
                break;
            }
        }

        // ----- General Case: Load Operands into Temporary Cells -----
        switch (opType)
        {
        case OE::NUM_SCALAR:
        {
            // LHS (number) -> T1_, RHS (scalar) -> T2_
            emit(VMI::SET, lhs.numberValue);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::NUM_ARR:
        {
            // LHS (number) -> T1_, RHS (array element) -> T2_
            emit(VMI::SET, lhs.numberValue);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::SCALAR_SCALAR:
        {
            // LHS (scalar) -> T1_, RHS (scalar) -> T2_
            loadScalar(*lhs.identifier.symbol);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::SCALAR_ARR:
        {
            loadScalar(*lhs.identifier.symbol);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::ARR_SCALAR:
        {
            loadArrayElement(lhs.identifier);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::ARR_ARR:
        {
            loadArrayElement(lhs.identifier);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        default:
            return;
        }
        // ----- Emit the Full Division Algorithm -----
        // check for zero divisor
        emit(VMI::SET, 1);
        emit(VMI::STORE, mapper.T9_);

        emit(VMI::LOAD, mapper.T2_);

        long long zeroDivIdx = emit(VMI::JZERO, 0); // placeholder for zero div jump

        // sign fix dvsr
        emit(VMI::JNEG, 4);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::STORE, mapper.T7_);
        emit(VMI::JUMP, 6);
        // dvsr neg
        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::STORE, mapper.T2_);
        emit(VMI::LOAD, mapper.T9_); // opt SET 1
        emit(VMI::STORE, mapper.T7_);

        // sgn fix dvdn
        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::JNEG, 4);
        // dvn neg
        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T8_);
        emit(VMI::JUMP, 6);
        // dvn pos
        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T1_);
        emit(VMI::LOAD, mapper.T9_); // opt SET 1
        emit(VMI::STORE, mapper.T8_);

        // initialize constant and working vars
        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::SET, 0);
        emit(VMI::STORE, mapper.T3_);

        // outer loop (while rem >= dvsr)
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::JNEG, 23);

        // init inner loop vars
        emit(VMI::LOAD, mapper.T2_);
        emit(VMI::STORE, mapper.T5_);
        emit(VMI::LOAD, mapper.T9_); // is 1 so eq set 1
        emit(VMI::STORE, mapper.T6_);

        // inner loop (while 2x <= rem)
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T5_);
        emit(VMI::SUB, mapper.T5_);
        emit(VMI::JNEG, 8);
        emit(VMI::LOAD, mapper.T5_);
        emit(VMI::ADD, mapper.T5_);
        emit(VMI::STORE, mapper.T5_);
        emit(VMI::LOAD, mapper.T6_);
        emit(VMI::ADD, mapper.T6_);
        emit(VMI::STORE, mapper.T6_);
        emit(VMI::JUMP, -10);
        // end inner loop
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T5_);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::ADD, mapper.T6_);
        emit(VMI::STORE, mapper.T3_);
        emit(VMI::JUMP, -24);

        // sign adjust for floor division
        emit(VMI::LOAD, mapper.T7_);
        emit(VMI::SUB, mapper.T8_);
        emit(VMI::JZERO, 10);
        // negative result
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::JZERO, 6);
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::ADD, mapper.T9_);
        emit(VMI::STORE, mapper.T3_);
        // fallthrough sign flip
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T3_);
        emit(VMI::STORE, mapper.T3_);

        // out quotient
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::JUMP, 2);

        // zero block
        patchJumpToCurrent(zeroDivIdx);

        emit(VMI::SET, 0);
    }
    long long CodeGenerator::python_mod(long long a, long long b, bool &isSafe)
    {
        // Compute the remainder using C++'s operator%
        long long r = a % b;

        // If r and b have different signs, adjust r
        if ((r < 0 && b > 0) || (r > 0 && b < 0))
        {
            // r += b;
            isSafe = safeops::safeAdd(r, b, r);
            if (!isSafe)
            {
                return 0;
            }
        }

        return r;
    }
    // Binary modulo (placeholder)
    void CodeGenerator::genPercent(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType)
    {
        using OE = OperandEnum;
        using VMI = VMInstruction;

        // Case: Both operands are numeric constants.
        if (opType == OE::NUM_NUM)
        {
            bool isSafe = false;
            long long result = python_mod(lhs.numberValue, rhs.numberValue, isSafe);
            if (isSafe)
            {
                emit(VMI::SET, result);
                return;
            }
            else
            {
                emit(VMI::SET, lhs.numberValue);
                emit(VMI::STORE, mapper.T1_);
                emit(VMI::SET, rhs.numberValue);
                emit(VMI::STORE, mapper.T2_);
            }
        }

        // ----- Constant Folding for Cases when RHS is a constant (for SCALAR_NUM or ARR_NUM) -----
        bool rhsIsConst = (opType == OE::SCALAR_NUM || opType == OE::ARR_NUM);
        if (rhsIsConst)
        {
            long long cstRHS = rhs.numberValue;
            switch (cstRHS)
            {
            case 0:
                // If either operand is zero (or divisor is zero), result is 0.
                emit(VMI::SET, 0);
                return;
            case 1:
                // For any dividend, a % 1 == 0.
                emit(VMI::SET, 0);
                return;
            case -1:
                // For any dividend, a % -1 == 0.
                emit(VMI::SET, 0);
                return;
            case 2:
            {
                // Compute a % 2.
                // For modulo 2, the remainder is 0 if a is even, 1 if odd.
                // We'll load the dividend (LHS), compute floor(a/2) using HALF,
                // then compute remainder = a - 2*(a//2).
                if (opType == OE::SCALAR_NUM)
                {
                    loadScalar(*lhs.identifier.symbol);
                }
                else
                { // OE::ARR_NUM
                    loadArrayElement(lhs.identifier);
                }
                emit(VMI::STORE, mapper.T1_); // T1_ = a
                emit(VMI::HALF);              // acc = floor(a/2)
                emit(VMI::ADD, 0);            // acc = 2*floor(a/2)
                emit(VMI::STORE, mapper.T2_);
                emit(VMI::LOAD, mapper.T1_); // load a
                emit(VMI::SUB, mapper.T2_);  // a - 2*floor(a/2) = a % 2
                return;
            }
            case -2:
            {
                if (opType == OE::SCALAR_NUM)
                {
                    loadScalar(*lhs.identifier.symbol);
                }
                else
                {
                    loadArrayElement(lhs.identifier);
                }
                emit(VMI::STORE, mapper.T1_);
                emit(VMI::HALF);
                emit(VMI::ADD, 0);
                emit(VMI::SUB, mapper.T1_);
                return;
            }
            default:
                // Not a special case constant; fall through to general code.
                break;
            }
        }

        // ----- General Case: Load Operands into Temporary Cells -----
        switch (opType)
        {
        case OE::NUM_SCALAR:
        {
            // LHS (number) -> T1_, RHS (scalar) -> T2_
            emit(VMI::SET, lhs.numberValue);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::NUM_ARR:
        {
            // LHS (number) -> T1_, RHS (array element) -> T2_
            emit(VMI::SET, lhs.numberValue);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::SCALAR_SCALAR:
        {
            // LHS (scalar) -> T1_, RHS (scalar) -> T2_
            loadScalar(*lhs.identifier.symbol);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::SCALAR_ARR:
        {
            loadScalar(*lhs.identifier.symbol);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::ARR_SCALAR:
        {
            loadArrayElement(lhs.identifier);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::ARR_ARR:
        {
            loadArrayElement(lhs.identifier);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        default:
            return;
        }
        // ----- Emit the Full Modulo Algorithm -----
        // check for zero divisor
        emit(VMI::SET, 1);
        emit(VMI::STORE, mapper.T9_);

        emit(VMI::LOAD, mapper.T2_);

        long long zeroDivIdx = emit(VMI::JZERO, 0); // placeholder for zero div jump

        // sign fix dvsr
        emit(VMI::JNEG, 4);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::STORE, mapper.T7_);
        emit(VMI::JUMP, 6);
        // dvsr neg
        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::STORE, mapper.T2_);
        emit(VMI::LOAD, mapper.T9_); // opt SET 1
        emit(VMI::STORE, mapper.T7_);

        // sgn fix dvdn
        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::JNEG, 4);
        // dvn neg
        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T8_);
        emit(VMI::JUMP, 6);
        // dvn pos
        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T1_);
        emit(VMI::LOAD, mapper.T9_); // opt SET 1
        emit(VMI::STORE, mapper.T8_);

        // initialize constant and working vars
        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::SET, 0);
        emit(VMI::STORE, mapper.T3_);

        // outer loop (while rem >= dvsr)
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::JNEG, 23);

        // init inner loop vars
        emit(VMI::LOAD, mapper.T2_);
        emit(VMI::STORE, mapper.T5_);
        emit(VMI::LOAD, mapper.T9_); // is 1 so eq set 1
        emit(VMI::STORE, mapper.T6_);

        // inner loop (while 2x <= rem)
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T5_);
        emit(VMI::SUB, mapper.T5_);
        emit(VMI::JNEG, 8);
        emit(VMI::LOAD, mapper.T5_);
        emit(VMI::ADD, mapper.T5_);
        emit(VMI::STORE, mapper.T5_);
        emit(VMI::LOAD, mapper.T6_);
        emit(VMI::ADD, mapper.T6_);
        emit(VMI::STORE, mapper.T6_);
        emit(VMI::JUMP, -10);
        // end inner loop
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T5_);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::ADD, mapper.T6_);
        emit(VMI::STORE, mapper.T3_);
        emit(VMI::JUMP, -24);

        // final remainder fix
        emit(VMI::LOAD, mapper.T7_);
        emit(VMI::SUB, mapper.T8_);
        emit(VMI::JZERO, 6);
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::JZERO, 6);
        emit(VMI::LOAD, mapper.T2_);
        emit(VMI::SUB, mapper.T4_);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::JUMP, 2);
        emit(VMI::LOAD, mapper.T4_);

        // adjust remainder sign if original divisor was negative
        emit(VMI::LOAD, mapper.T7_);
        emit(VMI::JZERO, 5);
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T4_);
        emit(VMI::STORE, mapper.T4_);

        // out remainder
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::JUMP, 2);
        // zero block
        patchJumpToCurrent(zeroDivIdx);

        emit(VMI::SET, 0);
    }

    // ===================================================
    // Array Address Calculation and Access
    // ===================================================
    void CodeGenerator::computeArrayElementAddr(const AST::Identifier &arrId, long long computedAddressCell)
    {
        auto arrSym = arrId.symbol;
        bool isRef = arrSym->isRefParam;

        if (isRef)
        {
            // For reference parameters, handle numeric and variable indices separately.
            if (arrId.idxType == AST::IdentifierIndexType::NUMERIC)
            {
                emit(VMInstruction::SET, arrId.indexNumber);
                emit(VMInstruction::ADDI, arrSym->address); // should point to offset cell
                emit(VMInstruction::STORE, computedAddressCell);
            }
            else
            {
                auto idxSym = arrId.indexVarSymbol;
                emit(VMInstruction::LOADI, arrSym->address); // should point to offset cell
                addScalar(*idxSym);
                emit(VMInstruction::STORE, computedAddressCell);
            }
            return;
        }
        else
        {
            // For non-reference arrays, handle numeric indices or variable indices.
            if (arrId.idxType == AST::IdentifierIndexType::NUMERIC)
            {
                long long base = arrSym->address;
                long long start = arrSym->arrayStart;
                long long idx = arrId.indexNumber;
                long long finalAddr = base + (idx - start);
                emit(VMInstruction::SET, finalAddr);
                emit(VMInstruction::STORE, computedAddressCell);
                return;
            }
            else
            {
                auto idxSym = arrId.indexVarSymbol;
                emit(VMInstruction::LOAD, arrSym->offsetCell);
                addScalar(*idxSym);
                emit(VMInstruction::STORE, computedAddressCell);
                return;
            }
        }
    }

    void CodeGenerator::loadArrayElement(const AST::Identifier &arrId)
    {
        // Compute the effective address and then load the value indirectly.
        computeArrayElementAddr(arrId, mapper.computedAddress_TMP_);
        emit(VMInstruction::LOADI, mapper.computedAddress_TMP_);
    }

    // ===================================================
    // Scalar Operations
    // ===================================================
    void CodeGenerator::loadScalar(const AST::Symbol &sym)
    {
        if (sym.isRefParam)
            emit(VMInstruction::LOADI, sym.address);
        else
            emit(VMInstruction::LOAD, sym.address);
    }

    void CodeGenerator::storeScalar(const AST::Symbol &sym)
    {
        if (sym.isRefParam)
            emit(VMInstruction::STOREI, sym.address);
        else
            emit(VMInstruction::STORE, sym.address);
    }

    void CodeGenerator::subScalar(const AST::Symbol &sym)
    {
        if (sym.isRefParam)
            emit(VMInstruction::SUBI, sym.address);
        else
            emit(VMInstruction::SUB, sym.address);
    }

    void CodeGenerator::addScalar(const AST::Symbol &sym)
    {
        if (sym.isRefParam)
            emit(VMInstruction::ADDI, sym.address);
        else
            emit(VMInstruction::ADD, sym.address);
    }

} // end namespace Compiler
