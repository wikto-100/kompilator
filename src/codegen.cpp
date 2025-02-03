/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */

#include "codegen.hpp"
#include "safeops.hpp"
#include <fstream>
#include <iostream>
#include <algorithm>
namespace Compiler
{

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

    long long CodeGenerator::emit(VMInstruction op)
    {
        CGInstruction instr{op, std::nullopt};
        m_code.push_back(instr);
        return (long long)m_code.size() - 1;
    }

    long long CodeGenerator::emit(VMInstruction op, long long operand)
    {
        CGInstruction instr{op, operand};
        m_code.push_back(instr);
        return (long long)m_code.size() - 1;
    }

    void CodeGenerator::backpatchOperand(long long instrIndex, long long newOperand)
    {
        if (instrIndex < 0 || instrIndex >= (long long)m_code.size())
        {
            std::cerr << "CodeGenerator: instrukcja poza zasięgiem.\n";
            return;
        }
        m_code[instrIndex].operand = newOperand;
    }

    void CodeGenerator::patchJumpToCurrent(long long jumpInstrIdx)
    {
        long long offset = (long long)m_code.size() - jumpInstrIdx;
        backpatchOperand(jumpInstrIdx, offset);
    }

    void CodeGenerator::writeCodeToFile(const std::string &filename)
    {
        std::ofstream out(filename);
        if (!out.is_open())
        {
            std::cerr << "CodeGenerator: nie można otworzyć pliku: " << filename << "\n";
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

        bool hasPut = std::any_of(m_code.begin(), m_code.end(), [](const CGInstruction &instr)
                                  { return instr.opcode == VMInstruction::PUT; });

        if (!hasPut)
        {
            m_code.erase(
                std::remove_if(m_code.begin(), m_code.end(), [](const CGInstruction &instr)
                               { return instr.opcode != VMInstruction::GET; }),
                m_code.end());
        }
    }

    void CodeGenerator::generate(const std::shared_ptr<AST::ProgramAll> &astRoot,
                                 const std::string &outputFilename)
    {
        if (!astRoot)
        {
            std::cerr << "CodeGenerator: null AST.\n";
            return;
        }

        mapper.assignAddresses(astRoot);

        genProgramAll(*astRoot);

        truncNoPUT(m_code);

        emit(VMInstruction::HALT);

        writeCodeToFile(outputFilename);
        std::cout << "CodeGenerator: zapisano kod do: " << outputFilename << "\n";
    }

    void CodeGenerator::genProgramAll(const AST::ProgramAll &prog)
    {

        long long skipToMainJump = 0;
        if (!prog.procedures.empty())
            skipToMainJump = emit(VMInstruction::JUMP, 0);

        for (auto &p : prog.procedures)
        {
            genProcedure(*p);
        }

        if (!prog.procedures.empty())
            patchJumpToCurrent(skipToMainJump);

        if (prog.mainPart)
        {
            genMain(*prog.mainPart);
        }
    }

    void CodeGenerator::genProcedure(const AST::Procedure &proc)
    {

        proc.procSymbol->address = static_cast<long long>(m_code.size());

        for (auto &d : proc.localDeclarations)
        {
            genDeclarations(*d);
        }
        genCommands(proc.commands);

        emit(VMInstruction::RTRN, proc.returnAddrCell);
    }

    void CodeGenerator::genMain(const AST::Main &m)
    {

        for (auto &d : m.declarations)
        {
            genDeclarations(*d);
        }

        genCommands(m.commands);
    }

    void CodeGenerator::genDeclarations(const AST::Declarations &decls)
    {

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

    void CodeGenerator::genAssign(const AST::AssignCmd &assign)
    {
        if (!assign.lhs.symbol)
        {
            std::cerr << "genAssign: LHS - brak symbolu\n";
            return;
        }

        if (assign.lhs.symbol->isArray)
        {
            computeArrayElementAddr(assign.lhs, mapper.computedAddress_RES_);
        }

        if (assign.rhs)
        {
            genExpression(assign.rhs);
        }

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

        genCondition(ifCmd.condition, jumpType);

        bool inverted = ifCmd.condition->op == AST::CondOp::EQ ||
                        ifCmd.condition->op == AST::CondOp::GT ||
                        ifCmd.condition->op == AST::CondOp::LT;
        if (!inverted)
        {

            long long condJumpIdx = emit(jumpType, 0);

            genCommands(ifCmd.thenCommands);

            long long jumpOverElseIdx = emit(VMInstruction::JUMP, 0);

            patchJumpToCurrent(condJumpIdx);

            genCommands(ifCmd.elseCommands);

            patchJumpToCurrent(jumpOverElseIdx);
        }
        else
        {

            long long condJumpIdx = emit(jumpType, 0);

            genCommands(ifCmd.elseCommands);
            long long jumpOverThenIdx = emit(VMInstruction::JUMP, 0);

            patchJumpToCurrent(condJumpIdx);

            genCommands(ifCmd.thenCommands);

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

            long long conditionStartIdx = (long long)m_code.size();
            genCondition(whileCmd.condition, jumpType);
            long long exitJumpIdx = emit(jumpType, 0);
            genCommands(whileCmd.body);

            long long jumpBackOffset = conditionStartIdx - (long long)m_code.size();
            emit(VMInstruction::JUMP, jumpBackOffset);

            patchJumpToCurrent(exitJumpIdx);
        }
        else
        {

            long long conditionStartIdx = (long long)m_code.size();
            genCondition(whileCmd.condition, jumpType);

            long long jumpOverExitIdx = emit(jumpType, 0);

            long long exitJumpIdx = emit(VMInstruction::JUMP, 0);

            long long loopBodyStartIdx = (long long)m_code.size();
            backpatchOperand(jumpOverExitIdx, loopBodyStartIdx - jumpOverExitIdx);

            genCommands(whileCmd.body);

            long long jumpBackOffset = conditionStartIdx - (long long)m_code.size();
            emit(VMInstruction::JUMP, jumpBackOffset);

            patchJumpToCurrent(exitJumpIdx);
        }
    }

    void CodeGenerator::genRepeat(const AST::RepeatCmd &repeatCmd)
    {

        long long loopStartIdx = (long long)m_code.size();

        genCommands(repeatCmd.body);

        VMInstruction jumpType = VMInstruction::JZERO;
        genCondition(repeatCmd.condition, jumpType);

        bool inverted = (repeatCmd.condition->op == AST::CondOp::EQ ||
                         repeatCmd.condition->op == AST::CondOp::GT ||
                         repeatCmd.condition->op == AST::CondOp::LT);

        if (!inverted)
        {

            long long jumpCondIdx = emit(jumpType, 0);

            long long jumpOffset = loopStartIdx - (long long)m_code.size();
            backpatchOperand(jumpCondIdx, jumpOffset);
        }
        else
        {

            long long jumpSkipLoopIdx = emit(jumpType, 0);

            long long jumpBackIdx = emit(VMInstruction::JUMP, 0);

            long long afterJumpIdx = (long long)m_code.size();
            backpatchOperand(jumpSkipLoopIdx, afterJumpIdx - jumpSkipLoopIdx);

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

            long long start = forCmd.startValue->numberValue;
            long long end = forCmd.endValue->numberValue;

            if (forCmd.downTo)
            {
                if (start < end)
                    return;
            }
            else
            {
                if (start > end)
                    return;
            }

            emit(VMInstruction::SET, start);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);

            long long condStartIdx = (long long)m_code.size();

            emit(VMInstruction::SET, end);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);
            break;
        }
        case OE::NUM_SCALAR:
        {

            long long start = forCmd.startValue->numberValue;

            emit(VMInstruction::SET, start);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);

            long long condStartIdx = (long long)m_code.size();
            loadScalar(*forCmd.endValue->identifier.symbol);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);

            break;
        }
        case OE::SCALAR_NUM:
        {

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

            long long start = forCmd.startValue->numberValue;
            computeArrayElementAddr(forCmd.endValue->identifier, mapper.computedAddress_TMP_);

            emit(VMInstruction::SET, start);
            emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);
            long long condStartIdx = (long long)m_code.size();

            emit(VMInstruction::LOADI, mapper.computedAddress_TMP_);
            emit(VMInstruction::SUB, forCmd.loopVarSymbol->address);

            forHelper(forCmd, condStartIdx);
            break;
        }
        case OE::ARR_SCALAR:
        {

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
            computeArrayElementAddr(forCmd.endValue->identifier, mapper.T1_);
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

        VMInstruction exitOpcode = forCmd.downTo ? VMInstruction::JPOS : VMInstruction::JNEG;
        long long jumpExitIdx = emit(exitOpcode, 0);

        genCommands(forCmd.body);

        long long step = forCmd.downTo ? -1 : 1;
        emit(VMInstruction::SET, step);
        emit(VMInstruction::ADD, forCmd.loopVarSymbol->address);
        emit(VMInstruction::STORE, forCmd.loopVarSymbol->address);

        long long jumpBackOffset = condStartIdx - (long long)m_code.size();
        emit(VMInstruction::JUMP, jumpBackOffset);

        patchJumpToCurrent(jumpExitIdx);
    }

    void CodeGenerator::genProcCall(const AST::ProcCallCmd &callCmd)
    {

        if (!callCmd.callee || !callCmd.callee->procSymbol)
        {
            std::cerr << "genProcCall: brak wywoływanej/symbolu\n";
            emit(VMInstruction::HALT);
            return;
        }

        auto calleeSymbol = callCmd.callee->procSymbol;

        for (size_t i = 0; i < callCmd.argSymbols.size(); i++)
        {
            auto argSym = callCmd.argSymbols[i];
            auto paramSym = callCmd.callee->paramSymbols[i];

            if (!argSym || !paramSym)
            {
                std::cerr << "genProcCall: brak symbolu arg/param\n";
                emit(VMInstruction::HALT);
                return;
            }

            if (argSym->isArray)
            {

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

        long long retIP = (long long)m_code.size() + 3;
        long long retCell = callCmd.callee->returnAddrCell;
        emit(VMInstruction::SET, retIP);
        emit(VMInstruction::STORE, retCell);

        long long currentIndex = (long long)m_code.size();

        long long procedureStartIndex = callCmd.callee->procSymbol->address;

        long long offset = procedureStartIndex - currentIndex;

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
                std::cerr << "genIO: readTarget brak symbolu.\n";
                return;
            }
            handleReadTarget(*ioCmd.readTarget);
        }
        else
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
            std::cerr << "handleReadTarget: brak symbolu.\n";
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
            std::cerr << "handleWriteTarget: brak symbolu.\n";
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

    void CodeGenerator::genExpression(const std::shared_ptr<AST::Expression> &expr)
    {
        if (!expr)
            return;

        if (expr->op == AST::ExprOp::NONE)
        {
            if (!expr->left)
            {
                std::cerr << "genExpression: brak lhs\n";
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
                    std::cerr << "genExpression: lhs/rhs - brak symbolu\n";
                }
            }
            else
            {

                emit(VMInstruction::SET, expr->left->numberValue);
            }
            return;
        }

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
            std::cerr << "genExpression: nieznana operacja\n";
            break;
        }
    }

    void CodeGenerator::genCondition(const std::shared_ptr<AST::Condition> &cond, VMInstruction &jumpType)
    {
        if (!cond || !cond->left || !cond->right)
        {
            std::cerr << "genCondition: brak lhs/rhs\n";
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
            std::cerr << "genCondition: nieznana operacja\n";
            break;
        }
    }

    void CodeGenerator::enumerateOperands(const AST::Value &lhs, const AST::Value &rhs, OperandEnum &opType)
    {

        if (!lhs.isIdentifier && !rhs.isIdentifier)
        {
            opType = OperandEnum::NUM_NUM;
            return;
        }

        if (lhs.isIdentifier && !rhs.isIdentifier)
        {
            auto symL = lhs.identifier.symbol;
            opType = (symL->isArray ? OperandEnum::ARR_NUM : OperandEnum::SCALAR_NUM);
            return;
        }

        if (!lhs.isIdentifier && rhs.isIdentifier)
        {
            auto symR = rhs.identifier.symbol;
            opType = (symR->isArray ? OperandEnum::NUM_ARR : OperandEnum::NUM_SCALAR);
            return;
        }

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

    void CodeGenerator::genStar(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType)
    {
        using OE = OperandEnum;
        using VMI = VMInstruction;

        auto emitSignFlip = [&]()
        {
            emit(VMI::STORE, mapper.T3_);
            emit(VMI::SET, 0);
            emit(VMI::SUB, mapper.T3_);
        };

        switch (opType)
        {
        case OE::NUM_NUM:
        {

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

        case OE::NUM_SCALAR:
        {
            const long long cst = lhs.numberValue;

            switch (cst)
            {
            case 0:

                emit(VMI::SET, 0);
                return;
            case 1:

                loadScalar(*rhs.identifier.symbol);
                return;
            case -1:

                loadScalar(*rhs.identifier.symbol);
                emitSignFlip();
                return;
            case 2:

                loadScalar(*rhs.identifier.symbol);
                emit(VMI::ADD, 0);
                return;
            case -2:
            {

                loadScalar(*rhs.identifier.symbol);
                emit(VMI::ADD, 0);
                emitSignFlip();
                return;
            }
            default:

                break;
            }

            emit(VMI::SET, cst);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }

        case OE::NUM_ARR:
        {
            const long long cst = lhs.numberValue;

            switch (cst)
            {
            case 0:
                emit(VMI::SET, 0);
                return;
            case 1:

                loadArrayElement(rhs.identifier);
                return;
            case -1:
                loadArrayElement(rhs.identifier);
                emitSignFlip();
                return;
            case 2:
            {

                loadArrayElement(rhs.identifier);
                emit(VMI::ADD, 0);
                return;
            }
            case -2:
            {

                loadArrayElement(rhs.identifier);
                emit(VMI::ADD, 0);
                emitSignFlip();
                return;
            }
            default:
                break;
            }

            emit(VMI::SET, cst);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }

        case OE::SCALAR_NUM:
        {
            const long long cst = rhs.numberValue;
            switch (cst)
            {
            case 0:

                emit(VMI::SET, 0);
                return;
            case 1:

                loadScalar(*lhs.identifier.symbol);
                return;
            case -1:
                loadScalar(*lhs.identifier.symbol);
                emitSignFlip();
                return;
            case 2:
            {

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

            loadArrayElement(lhs.identifier);
            emit(VMI::STORE, mapper.T1_);
            emit(VMI::SET, cst);
            emit(VMI::STORE, mapper.T2_);
            break;
        }

        case OE::SCALAR_SCALAR:
        {

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

        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T3_);

        emit(VMI::LOAD, mapper.T2_);
        emit(VMI::JNEG, 2);
        emit(VMI::JUMP, 7);

        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T1_);
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::STORE, mapper.T2_);

        emit(VMI::LOAD, mapper.T2_);
        emit(VMI::JZERO, 15);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::HALF);
        emit(VMI::STORE, mapper.T2_);
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::JZERO, 4);
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::ADD, mapper.T1_);
        emit(VMI::STORE, mapper.T3_);
        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::ADD, mapper.T1_);
        emit(VMI::STORE, mapper.T1_);
        emit(VMI::JUMP, -15);

        emit(VMI::LOAD, mapper.T3_);
    }
    long long CodeGenerator::python_floor_div(long long a, long long b, bool &isSafe)
    {
        if (b == 0 || a == 0)
        {
            return 0;
        }

        long long quotient;
        isSafe = safeops::safeDivide(a, b, quotient);
        if (quotient == LLONG_MIN)
        {
            isSafe = false;
            return 0;
        }

        long long remainder = a % b;

        if (remainder != 0 && ((a < 0) != (b < 0)))
        {
            quotient -= 1;
        }

        return quotient;
    }

    void CodeGenerator::genSlash(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType)
    {

        using OE = OperandEnum;
        using VMI = VMInstruction;

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

        bool rhsIsConst = (opType == OE::SCALAR_NUM || opType == OE::ARR_NUM);
        if (rhsIsConst)
        {
            long long cstRHS = rhs.numberValue;
            switch (cstRHS)
            {
            case 0:

                emit(VMI::SET, 0);
                return;
            case 1:

                if (opType == OE::SCALAR_NUM)
                {
                    loadScalar(*lhs.identifier.symbol);
                }
                else
                {
                    loadArrayElement(lhs.identifier);
                }
                return;
            case -1:

                if (opType == OE::SCALAR_NUM)
                {
                    loadScalar(*lhs.identifier.symbol);
                }
                else
                {
                    loadArrayElement(lhs.identifier);
                }
                {

                    emit(VMI::STORE, mapper.T3_);
                    emit(VMI::SET, 0);
                    emit(VMI::SUB, mapper.T3_);
                }
                return;
            case 2:

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

                    emit(VMI::STORE, mapper.T3_);
                    emit(VMI::SET, 0);
                    emit(VMI::SUB, mapper.T3_);
                }
                return;
            default:

                break;
            }
        }

        switch (opType)
        {
        case OE::NUM_SCALAR:
        {

            emit(VMI::SET, lhs.numberValue);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::NUM_ARR:
        {

            emit(VMI::SET, lhs.numberValue);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::SCALAR_SCALAR:
        {

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

        emit(VMI::SET, 1);
        emit(VMI::STORE, mapper.T9_);

        emit(VMI::LOAD, mapper.T2_);

        long long zeroDivIdx = emit(VMI::JZERO, 0);

        emit(VMI::JNEG, 4);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::STORE, mapper.T7_);
        emit(VMI::JUMP, 6);

        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::STORE, mapper.T2_);
        emit(VMI::LOAD, mapper.T9_);
        emit(VMI::STORE, mapper.T7_);

        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::JNEG, 4);

        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T8_);
        emit(VMI::JUMP, 6);

        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T1_);
        emit(VMI::LOAD, mapper.T9_);
        emit(VMI::STORE, mapper.T8_);

        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::SET, 0);
        emit(VMI::STORE, mapper.T3_);

        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::JNEG, 23);

        emit(VMI::LOAD, mapper.T2_);
        emit(VMI::STORE, mapper.T5_);
        emit(VMI::LOAD, mapper.T9_);
        emit(VMI::STORE, mapper.T6_);

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

        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T5_);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::ADD, mapper.T6_);
        emit(VMI::STORE, mapper.T3_);
        emit(VMI::JUMP, -24);

        emit(VMI::LOAD, mapper.T7_);
        emit(VMI::SUB, mapper.T8_);
        emit(VMI::JZERO, 10);

        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::JZERO, 6);
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::ADD, mapper.T9_);
        emit(VMI::STORE, mapper.T3_);

        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T3_);
        emit(VMI::STORE, mapper.T3_);

        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::JUMP, 2);

        patchJumpToCurrent(zeroDivIdx);

        emit(VMI::SET, 0);
    }
    long long CodeGenerator::python_mod(long long a, long long b, bool &isSafe)
    {

        long long r = a % b;

        if ((r < 0 && b > 0) || (r > 0 && b < 0))
        {

            isSafe = safeops::safeAdd(r, b, r);
            if (!isSafe)
            {
                return 0;
            }
        }

        return r;
    }

    void CodeGenerator::genPercent(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType)
    {
        using OE = OperandEnum;
        using VMI = VMInstruction;

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

        bool rhsIsConst = (opType == OE::SCALAR_NUM || opType == OE::ARR_NUM);
        if (rhsIsConst)
        {
            long long cstRHS = rhs.numberValue;
            switch (cstRHS)
            {
            case 0:

                emit(VMI::SET, 0);
                return;
            case 1:

                emit(VMI::SET, 0);
                return;
            case -1:

                emit(VMI::SET, 0);
                return;
            case 2:
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
                emit(VMI::STORE, mapper.T2_);
                emit(VMI::LOAD, mapper.T1_);
                emit(VMI::SUB, mapper.T2_);
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

                break;
            }
        }

        switch (opType)
        {
        case OE::NUM_SCALAR:
        {

            emit(VMI::SET, lhs.numberValue);
            emit(VMI::STORE, mapper.T1_);
            loadScalar(*rhs.identifier.symbol);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::NUM_ARR:
        {

            emit(VMI::SET, lhs.numberValue);
            emit(VMI::STORE, mapper.T1_);
            loadArrayElement(rhs.identifier);
            emit(VMI::STORE, mapper.T2_);
            break;
        }
        case OE::SCALAR_SCALAR:
        {

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

        emit(VMI::SET, 1);
        emit(VMI::STORE, mapper.T9_);

        emit(VMI::LOAD, mapper.T2_);

        long long zeroDivIdx = emit(VMI::JZERO, 0);

        emit(VMI::JNEG, 4);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::STORE, mapper.T7_);
        emit(VMI::JUMP, 6);

        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::STORE, mapper.T2_);
        emit(VMI::LOAD, mapper.T9_);
        emit(VMI::STORE, mapper.T7_);

        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::JNEG, 4);

        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T8_);
        emit(VMI::JUMP, 6);

        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T1_);
        emit(VMI::STORE, mapper.T1_);
        emit(VMI::LOAD, mapper.T9_);
        emit(VMI::STORE, mapper.T8_);

        emit(VMI::LOAD, mapper.T1_);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::SET, 0);
        emit(VMI::STORE, mapper.T3_);

        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T2_);
        emit(VMI::JNEG, 23);

        emit(VMI::LOAD, mapper.T2_);
        emit(VMI::STORE, mapper.T5_);
        emit(VMI::LOAD, mapper.T9_);
        emit(VMI::STORE, mapper.T6_);

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

        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SUB, mapper.T5_);
        emit(VMI::STORE, mapper.T4_);
        emit(VMI::LOAD, mapper.T3_);
        emit(VMI::ADD, mapper.T6_);
        emit(VMI::STORE, mapper.T3_);
        emit(VMI::JUMP, -24);

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

        emit(VMI::LOAD, mapper.T7_);
        emit(VMI::JZERO, 5);
        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::SET, 0);
        emit(VMI::SUB, mapper.T4_);
        emit(VMI::STORE, mapper.T4_);

        emit(VMI::LOAD, mapper.T4_);
        emit(VMI::JUMP, 2);

        patchJumpToCurrent(zeroDivIdx);

        emit(VMI::SET, 0);
    }

    void CodeGenerator::computeArrayElementAddr(const AST::Identifier &arrId, long long computedAddressCell)
    {
        auto arrSym = arrId.symbol;
        bool isRef = arrSym->isRefParam;

        if (isRef)
        {
            if (arrId.idxType == AST::IdentifierIndexType::NUMERIC)
            {
                emit(VMInstruction::SET, arrId.indexNumber);
                emit(VMInstruction::ADDI, arrSym->address);
                emit(VMInstruction::STORE, computedAddressCell);
            }
            else
            {
                auto idxSym = arrId.indexVarSymbol;
                emit(VMInstruction::LOADI, arrSym->address);
                addScalar(*idxSym);
                emit(VMInstruction::STORE, computedAddressCell);
            }
            return;
        }
        else
        {
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
        computeArrayElementAddr(arrId, mapper.computedAddress_TMP_);
        emit(VMInstruction::LOADI, mapper.computedAddress_TMP_);
    }

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

}