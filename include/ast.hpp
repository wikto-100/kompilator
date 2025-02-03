/**
 * \author: Wiktor Stojek nr. indeksu 272383
 */
#pragma once

#include <string>
#include <vector>
#include <memory>
#include <optional>

namespace Compiler
{

    /**
     * \brief Drzewo składniowe abstrakcyjne (AST) dla języka maszyny wirtualnej.
     *
     */
    class AST
    {
    public:
        // ----------------------------------------------------------------------
        // Symbol
        // ----------------------------------------------------------------------
        struct Symbol
        {
            std::string name;           // nazwa symbolu
            bool isProcedure = false;   // czy symbol jest procedurą
            bool isArray = false;       // czy symbol jest tablicą
            bool isRefParam = false;    // czy symbol jest parametrem referencyjnym
            bool isIterator = false;    // czy symbol jest iteratorem pętli FOR
            long long arrayStart = 0;   // wartość początku tablicy
            long long arrayEnd = 0;     // wartość końca tablicy
            long long address = 0;      // komórka pamięci maszyny wirtualnej
            long long offsetCell = 0;   // komórka pamięci przechowująca przesunięcie tablicy
            bool isInitialized = false; // czy zmienna została zainicjalizowana
        };

        // ----------------------------------------------------------------------
        // Deklaracje w przód
        // ----------------------------------------------------------------------
        struct ProgramAll;
        struct Procedure;
        struct Main;
        struct Declarations;
        struct Command;
        struct Expression;
        struct Condition;
        struct Value;

        // ----------------------------------------------------------------------
        // Identyfikatory
        // ----------------------------------------------------------------------
        enum class IdentifierIndexType
        {
            NONE,    // brak indeksu
            NUMERIC, // indeks stały
            VARIABLE // indeks zmienny
        };

        struct Identifier
        {
            std::string name;                                        // Nazwa identyfikatora
            IdentifierIndexType idxType = IdentifierIndexType::NONE; // Typ indeksu (jeśli tablica)
            long long indexNumber = 0;                               // Wartość indeksu (jeśli idxType == NUMERIC)
            std::string indexVar;                                    // Nazwa zmiennej indeksującej (jeśli idxType == VARIABLE)
            std::shared_ptr<Symbol> indexVarSymbol;                  // Wskaźnik indeksu do tablicy symboli
            std::shared_ptr<Symbol> symbol;                          // Wskaźnik na symbol identyfikatora
            int line = 0;                                            // Numer linii kodu źródłowego
            int column = 0;                                          // Numer kolumny kodu źródłowego
        };

        // ----------------------------------------------------------------------
        // Wartości
        // ----------------------------------------------------------------------
        /**
         * \brief Wartość: stała liczba lub identyfikator.
         */
        struct Value
        {
            bool isIdentifier = false;
            long long numberValue = 0; // jeśli isIdentifier == false
            Identifier identifier;     // jeśli isIdentifier == true
        };

        // ----------------------------------------------------------------------
        // Wyrażenia
        // ----------------------------------------------------------------------
        enum class ExprOp
        {
            NONE,
            PLUS,
            MINUS,
            STAR,
            SLASH,
            PERCENT
        };

        /**
         * \brief Wyrażenie binarne z operatorem i lewą/prawą wartością.
         */
        struct Expression
        {
            ExprOp op = ExprOp::NONE;     // Operator
            std::shared_ptr<Value> left;  // Lewa wartość (zawsze używana, nawet jeśli op == NONE)
            std::shared_ptr<Value> right; // Prawa wartość (używana tylko jeśli op != NONE)
        };

        // ----------------------------------------------------------------------
        // Warunki
        // ----------------------------------------------------------------------
        enum class CondOp
        {
            EQ,
            NEQ,
            GT,
            LT,
            GEQ,
            LEQ
        };

        /**
         * \brief Warunek porównania dwóch wartości.
         */
        struct Condition
        {
            CondOp op;                    // Operator porównania
            std::shared_ptr<Value> left;  // Lewa wartość
            std::shared_ptr<Value> right; // Prawa wartość
        };

        // ----------------------------------------------------------------------
        // Polecenia wejścia/wyjścia
        // ----------------------------------------------------------------------
        enum class IOType
        {
            READ,
            WRITE
        };

        /**
         * \brief Reprezentuje polecenie wejścia/wyjścia (READ/WRITE).
         */
        struct IOCommand
        {
            IOType ioType;                                    // Typ polecenia
            std::optional<Identifier> readTarget;             // tylko dla READ (identifier)
            std::optional<std::shared_ptr<Value>> writeValue; // tylko dla WRITE (value)
        };

        // ----------------------------------------------------------------------
        // Polecenia
        // ----------------------------------------------------------------------
        enum class CommandType
        {
            ASSIGN,
            IF_THEN,
            IF_THEN_ELSE,
            WHILE,
            REPEAT,
            FOR_TO,
            FOR_DOWNTO,
            PROC_CALL,
            READ,
            WRITE
        };

        // Deklaracje w przód
        struct AssignCmd;
        struct IfCmd;
        struct WhileCmd;
        struct RepeatCmd;
        struct ForCmd;
        struct ProcCallCmd;

        /**
         * \brief Polecenia w programie.
         */
        struct Command
        {
            CommandType type;

            std::shared_ptr<AssignCmd> assignCmd;     // przypisanie
            std::shared_ptr<IfCmd> ifCmd;             // instrukcja warunkowa IF
            std::shared_ptr<WhileCmd> whileCmd;       // pętla WHILE
            std::shared_ptr<RepeatCmd> repeatCmd;     // pętla REPEAT-UNTIL
            std::shared_ptr<ForCmd> forCmd;           // pętla FOR
            std::shared_ptr<ProcCallCmd> procCallCmd; // wywołanie procedury
            std::shared_ptr<IOCommand> ioCmd;         // polecenie wejścia/wyjścia
        };

        // ----------------------------------------------------------------------
        // Struktury konkretnych poleceń
        // ----------------------------------------------------------------------
        struct AssignCmd
        {
            Identifier lhs;                  // lewa strona przypisania
            std::shared_ptr<Expression> rhs; // wyrażenie przypisywane do lhs
        };

        struct IfCmd
        {
            std::shared_ptr<Condition> condition;               // warunek IF
            std::vector<std::shared_ptr<Command>> thenCommands; // polecenia w IF
            std::vector<std::shared_ptr<Command>> elseCommands; // polecenia w ELSE
        };

        struct WhileCmd
        {
            std::shared_ptr<Condition> condition;       // warunek pętli
            std::vector<std::shared_ptr<Command>> body; // polecenia w pętli
        };

        struct RepeatCmd
        {
            std::vector<std::shared_ptr<Command>> body; // polecenia w pętli
            std::shared_ptr<Condition> condition;       // warunek pętli REPEAT-UNTIL
        };

        struct ForCmd
        {
            std::string loopVar;                        // nazwa zmiennej pętli
            bool downTo = false;                        // czy pętla ma być malejąca
            std::shared_ptr<Value> startValue;          // wartość początku pętli
            std::shared_ptr<Value> endValue;            // wartość końca pętli
            std::vector<std::shared_ptr<Command>> body; // polecenia w pętli

            std::shared_ptr<Symbol> loopVarSymbol; // konwertowane z loopVar
            long long helperCell = 0;              // używana w przypadku pętli FORUP
        };

        struct ProcCallCmd
        {
            std::string name;                                // nazwa procedury
            std::vector<std::string> args;                   // argumenty procedury
            std::vector<std::shared_ptr<Symbol>> argSymbols; // konwertowane z args
            std::shared_ptr<Procedure> callee;               // wywołana procedura
        };

        // ----------------------------------------------------------------------
        // Deklaracje
        // ----------------------------------------------------------------------
        struct DeclItem
        {
            std::string name;               // nazwa deklarowanej zmiennej
            bool isArray = false;           // czy zmienna jest tablicą
            long long rangeStart = 0;       // początek zakresu tablicy (jeśli tablica)
            long long rangeEnd = 0;         // koniec zakresu tablicy (jeśli tablica)
            std::shared_ptr<Symbol> symbol; // wskaźnik na symbol deklarowanej zmiennej
        };

        struct Declarations
        {
            std::vector<DeclItem> items; // deklaracje zmiennych
        };

        // ----------------------------------------------------------------------
        // Procedury
        // ----------------------------------------------------------------------
        struct Procedure
        {
            std::string name;                                             // nazwa procedury
            std::vector<std::pair<bool, std::string>> arguments;          // argumenty procedury
            std::vector<std::shared_ptr<Symbol>> paramSymbols;            // symbole parametrów procedury
            std::vector<std::shared_ptr<Declarations>> localDeclarations; // deklaracje lokalne procedury
            std::vector<std::shared_ptr<Command>> commands;               // polecenia procedury
            long long returnAddrCell = 0;                                 // komórka pamięci z adresem powrotu
            std::shared_ptr<Symbol> procSymbol;                           // symbol procedury
        };

        // ----------------------------------------------------------------------
        // Main
        // ----------------------------------------------------------------------
        struct Main
        {
            std::vector<std::shared_ptr<Declarations>> declarations; // deklaracje zmiennych
            std::vector<std::shared_ptr<Command>> commands;          // polecenia programu głównego
        };

        // ----------------------------------------------------------------------
        // ProgramAll
        // ----------------------------------------------------------------------
        struct ProgramAll
        {
            std::vector<std::shared_ptr<Procedure>> procedures; // procedury
            std::shared_ptr<Main> mainPart;                     // program główny
        };
    };

} // namespace Compiler
