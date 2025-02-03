#pragma once

#include <string>
#include <vector>
#include <memory>
#include <optional>

namespace Compiler
{

    /**
     * \brief The top-level container for all AST nodes,
     *        including Symbol, ProgramAll, Procedure, etc.
     *
     * Everything is inside `class AST` to group them.
     */
    class AST
    {
    public:
        // ----------------------------------------------------------------------
        // Symbol
        // ----------------------------------------------------------------------
        struct Symbol
        {
            std::string name;
            bool isProcedure = false;
            bool isArray = false;
            bool isRefParam = false;
            bool isIterator = false;
            long long arrayStart = 0;
            long long arrayEnd = 0;
            long long address = 0;
            long long offsetCell = 0;  // used if isArray==true
            bool isInitialized = false;
        };

        // ----------------------------------------------------------------------
        // Forward Declarations
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
        // Static Dump Function
        // ----------------------------------------------------------------------
        /**
         * \brief Dump (print) the entire AST for debugging.
         * \param root A const reference to the root ProgramAll node.
         *
         * This function does not modify the AST; it only reads and prints it.
         */
        static void DumpAST(const ProgramAll &root);

        // ----------------------------------------------------------------------
        // Identifier
        // ----------------------------------------------------------------------
        enum class IdentifierIndexType
        {
            NONE,
            NUMERIC,
            VARIABLE
        };

        struct Identifier
        {
            std::string name;
            IdentifierIndexType idxType = IdentifierIndexType::NONE;
            long long indexNumber = 0;              ///< Valid if idxType == NUMERIC
            std::string indexVar;                   ///< Valid if idxType == VARIABLE
            std::shared_ptr<Symbol> indexVarSymbol; ///< Optional pointer to index variable symbol
            std::shared_ptr<Symbol> symbol;         ///< Optional pointer to symbol table entry
            int line = 0;                           ///< Source code line
            int column = 0;                         ///< Source code column
        };

        // ----------------------------------------------------------------------
        // Value
        // ----------------------------------------------------------------------
        /**
         * \brief Represents either a numeric constant or an identifier.
         */
        struct Value
        {
            bool isIdentifier = false;
            long long numberValue = 0; ///< valid if !isIdentifier
            Identifier identifier;     ///< valid if isIdentifier
        };

        // ----------------------------------------------------------------------
        // Expression
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
         * \brief An expression node: left op right, or just a single Value if op=NONE.
         */
        struct Expression
        {
            ExprOp op = ExprOp::NONE;
            std::shared_ptr<Value> left;  ///< Always used
            std::shared_ptr<Value> right; ///< Used only if op != NONE
        };

        // ----------------------------------------------------------------------
        // Condition
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
         * \brief A condition node with an operator and left/right Value.
         */
        struct Condition
        {
            CondOp op;
            std::shared_ptr<Value> left;
            std::shared_ptr<Value> right;
        };

        // ----------------------------------------------------------------------
        // IO Command
        // ----------------------------------------------------------------------
        enum class IOType
        {
            READ,
            WRITE
        };

        /**
         * \brief A read or write command structure.
         *        readTarget is used if ioType=READ; writeValue if ioType=WRITE.
         */
        struct IOCommand
        {
            IOType ioType;
            std::optional<Identifier> readTarget;
            std::optional<std::shared_ptr<Value>> writeValue;
        };

        // ----------------------------------------------------------------------
        // Command
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

        // Forward declarations of command-specific structs
        struct AssignCmd;
        struct IfCmd;
        struct WhileCmd;
        struct RepeatCmd;
        struct ForCmd;
        struct ProcCallCmd;

        /**
         * \brief A generic command node that can hold various sub-commands.
         */
        struct Command
        {
            CommandType type;

            std::shared_ptr<AssignCmd> assignCmd;
            std::shared_ptr<IfCmd> ifCmd;
            std::shared_ptr<WhileCmd> whileCmd;
            std::shared_ptr<RepeatCmd> repeatCmd;
            std::shared_ptr<ForCmd> forCmd;
            std::shared_ptr<ProcCallCmd> procCallCmd;
            std::shared_ptr<IOCommand> ioCmd;
        };

        // ----------------------------------------------------------------------
        // Command-specific structs
        // ----------------------------------------------------------------------
        struct AssignCmd
        {
            Identifier lhs;
            std::shared_ptr<Expression> rhs;
        };

        struct IfCmd
        {
            std::shared_ptr<Condition> condition;
            std::vector<std::shared_ptr<Command>> thenCommands;
            std::vector<std::shared_ptr<Command>> elseCommands;

        };

        struct WhileCmd
        {
            std::shared_ptr<Condition> condition;
            std::vector<std::shared_ptr<Command>> body;
        };

        struct RepeatCmd
        {
            std::vector<std::shared_ptr<Command>> body;
            std::shared_ptr<Condition> condition;
        };

        struct ForCmd
        {
            std::string loopVar;
            bool downTo = false;
            std::shared_ptr<Value> startValue;
            std::shared_ptr<Value> endValue;
            std::vector<std::shared_ptr<Command>> body;

            std::shared_ptr<Symbol> loopVarSymbol;
            long long helperCell = 0; // used for loop variable
        };

        struct ProcCallCmd
        {
            std::string name;
            std::vector<std::string> args;
            std::vector<std::shared_ptr<Symbol>> argSymbols;
            std::shared_ptr<Procedure> callee;
        };

        // ----------------------------------------------------------------------
        // Declarations
        // ----------------------------------------------------------------------
        struct DeclItem
        {
            std::string name;
            bool isArray = false;
            long long rangeStart = 0;
            long long rangeEnd = 0;
            std::shared_ptr<Symbol> symbol;
        };

        struct Declarations
        {
            std::vector<DeclItem> items;
        };

        // ----------------------------------------------------------------------
        // Procedure
        // ----------------------------------------------------------------------
        struct Procedure
        {
            std::string name;
            std::vector<std::pair<bool, std::string>> arguments; // gets converted to symbols
            std::vector<std::shared_ptr<Symbol>> paramSymbols;
            std::vector<std::shared_ptr<Declarations>> localDeclarations;
            std::vector<std::shared_ptr<Command>> commands;
            long long returnAddrCell = 0; // used for procedure return address
            std::shared_ptr<Symbol> procSymbol;
        };

        // ----------------------------------------------------------------------
        // Main
        // ----------------------------------------------------------------------
        struct Main
        {
            std::vector<std::shared_ptr<Declarations>> declarations;
            std::vector<std::shared_ptr<Command>> commands;
        };

        // ----------------------------------------------------------------------
        // ProgramAll
        // ----------------------------------------------------------------------
        struct ProgramAll
        {
            std::vector<std::shared_ptr<Procedure>> procedures;
            std::shared_ptr<Main> mainPart;
        };

        static std::string indentStr(int indent);

    private:
        // Private helper methods for DumpAST (implemented in ast.cpp).
        static void dumpProgramAll(const ProgramAll &prog, int indent);
        static void dumpProcedure(const Procedure &proc, int indent);
        static void dumpMain(const Main &m, int indent);
        static void dumpDeclarations(const Declarations &decls, int indent);
        static void dumpCommand(const Command &cmd, int indent);
        static void dumpExpression(const Expression &expr, int indent);
        static void dumpCondition(const Condition &cond, int indent);
        static void dumpValue(const Value &val, int indent);
        static void dumpIdentifier(const Identifier &id, int indent);
    };

} // namespace Compiler
