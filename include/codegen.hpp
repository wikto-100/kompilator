/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
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
    // Instrukcje Maszyny Wirtualnej
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
    // Typy Operandów
    // ===================================================
    enum class OperandEnum
    {
        NUM_NUM,       // stała - stała
        NUM_SCALAR,    // stała - zmienna
        SCALAR_NUM,    // zmienna - stała
        SCALAR_SCALAR, // zmienna - zmienna
        ARR_NUM,       // tablica - stała
        NUM_ARR,       // stała - tablica
        ARR_SCALAR,    // tablica - zmienna
        SCALAR_ARR,    // zmienna - tablica
        ARR_ARR,       // tablica - tablica
        NONE           // brak (??)
    };

    // ===================================================
    // Struktura generowanych instrukcji
    // ===================================================
    struct CGInstruction
    {
        VMInstruction opcode;             // Kod operacji
        std::optional<long long> operand; // argument operacji (opcjonalny)
    };

    // ===================================================
    // Klasa Generatora Kodu
    // ===================================================
    class CodeGenerator
    {
    public:
        CodeGenerator() = default;
        ~CodeGenerator() = default;

        // ------------------------------------
        // Interfejs publiczny
        // ------------------------------------
        // Generuje kod dla całego programu
        void generate(const std::shared_ptr<AST::ProgramAll> &astRoot,
                      const std::string &outputFilename);

    private:
        // ===================================================
        // Funkcje optymalizujące kod
        // ===================================================
        void truncNoPUT(std::vector<CGInstruction> &m_code);

        // ===================================================
        // Funkcje emitujące instrukcje
        // ===================================================

        // Zapisuje instrukcję VM bez argumentu
        long long emit(VMInstruction op);
        // Zapisuje instrukcję VM z argumentem
        long long emit(VMInstruction op, long long operand);
        // Zmienia argument instrukcji o indeksie instrIndex na newOperand
        void backpatchOperand(long long instrIndex, long long newOperand);
        // Zmienia argument skoku do instrukcji o indeksie jumpInstrIdx na aktualny indeks
        void patchJumpToCurrent(long long jumpInstrIdx);

        // ===================================================
        // Zapisuje kod do pliku
        void writeCodeToFile(const std::string &filename);

        // ===================================================
        // Funkcje przejścia po AST
        // ===================================================
        // Generuje kod dla całego programu
        void genProgramAll(const AST::ProgramAll &prog);
        // Generuje kod dla pojedynczej procedury
        void genProcedure(const AST::Procedure &proc);
        // Generuje kod dla main
        void genMain(const AST::Main &m);
        // Generuje deklaracje
        void genDeclarations(const AST::Declarations &decls);
        // Generuje pojedynczą deklarację
        void genCommands(const std::vector<std::shared_ptr<AST::Command>> &cmds);
        // Generuje pojedyncze polecenie
        void genCommand(const AST::Command &cmd);

        // ===================================================
        // Generowanie kodu dla poszczególnych instrukcji
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
        // Generowanie kodu dla wyrażeń i warunków
        // ===================================================
        void genExpression(const std::shared_ptr<AST::Expression> &expr);
        void genCondition(const std::shared_ptr<AST::Condition> &cond, VMInstruction &jumpType);

        // ===================================================
        // Funkcje generujące binarne operacje arytmetyczne
        // ===================================================
        // Wylicza typ argumentów operacji
        void enumerateOperands(const AST::Value &lhs, const AST::Value &rhs, OperandEnum &opType);
        void genPlus(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        void genMinus(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        void genStar(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        void genSlash(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        void genPercent(const AST::Value &lhs, const AST::Value &rhs, const OperandEnum &opType);
        // Funkcje pomocnicze dla operacji arytmetycznych
        long long python_floor_div(long long a, long long b, bool &isSafe);
        long long python_mod(long long a, long long b, bool &isSafe);

        // ===================================================
        // Funckje pomocnicze dla operacji wejścia/wyjścia
        void handleReadTarget(const AST::Identifier &id);
        void handleWriteTarget(const AST::Identifier &id);

        // ===================================================
        // Funkcja obliczająca adres elementu tablicy -> p[computedAddr]
        void computeArrayElementAddr(const AST::Identifier &arrId, long long computedAddr);
        // Funkcja ładująca element tablicy do akumulatora p[0]
        void loadArrayElement(const AST::Identifier &arrId);

        // Funkcje pomocnicze dla operacji na zmiennych
        void loadScalar(const AST::Symbol &sym);
        void storeScalar(const AST::Symbol &sym);
        void subScalar(const AST::Symbol &sym);
        void addScalar(const AST::Symbol &sym);

    private:
        // Instancja klasy mapującej pamięć maszyny wirtualnej do zmiennych
        MemoryMapper mapper;
        // Wektor wygenerowanych instrukcji
        std::vector<CGInstruction> m_code;
    };

} // namespace Compiler
