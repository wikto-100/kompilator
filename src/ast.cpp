#include "ast.hpp"
#include <iostream> // for std::cout

namespace Compiler {

// Forward declaration of our new symbol-dumping helper
static void dumpSymbol(const std::shared_ptr<AST::Symbol> &sym, int indent);

// Static method: DumpAST
void AST::DumpAST(const ProgramAll& root)
{
    dumpProgramAll(root, 0);
}

////////////////////////////////////////////////////////////////////////////////
// Private helper methods
////////////////////////////////////////////////////////////////////////////////

std::string AST::indentStr(int indent)
{
    return std::string(indent, ' ');
}

void AST::dumpProgramAll(const ProgramAll& prog, int indent)
{
    std::cout << indentStr(indent) << "ProgramAll:\n";

    if (!prog.procedures.empty()) {
        std::cout << indentStr(indent + 2) << "Procedures:\n";
        for (auto &p : prog.procedures) {
            dumpProcedure(*p, indent + 4);
        }
    }

    if (prog.mainPart) {
        std::cout << indentStr(indent + 2) << "Main:\n";
        dumpMain(*prog.mainPart, indent + 4);
    }
}

void AST::dumpProcedure(const Procedure& proc, int indent)
{
    std::cout << indentStr(indent) << "Procedure: " << proc.name << "\n";

    // If there's a 'proc.symbol', you could dump it here:
    if (proc.procSymbol) {
        std::cout << indentStr(indent + 2) << "(Procedure Symbol):\n";
        dumpSymbol(proc.procSymbol, indent + 4);
    }

    if (!proc.arguments.empty()) {
        std::cout << indentStr(indent + 2) << "Args:\n";
        for (auto &arg : proc.arguments) {
            bool isArrayParam = arg.first;
            const std::string &argName = arg.second;
            std::cout << indentStr(indent + 4)
                      << (isArrayParam ? "T " : "")
                      << argName << "\n";
        }
    }

    if (!proc.localDeclarations.empty()) {
        std::cout << indentStr(indent + 2) << "Local Declarations:\n";
        for (auto &decls : proc.localDeclarations) {
            dumpDeclarations(*decls, indent + 4);
        }
    }

    if (!proc.commands.empty()) {
        std::cout << indentStr(indent + 2) << "Commands:\n";
        for (auto &cmd : proc.commands) {
            dumpCommand(*cmd, indent + 4);
        }
    }
}

void AST::dumpMain(const Main& m, int indent)
{
    if (!m.declarations.empty()) {
        std::cout << indentStr(indent) << "Declarations:\n";
        for (auto &decls : m.declarations) {
            dumpDeclarations(*decls, indent + 2);
        }
    }
    if (!m.commands.empty()) {
        std::cout << indentStr(indent) << "Commands:\n";
        for (auto &cmd : m.commands) {
            dumpCommand(*cmd, indent + 2);
        }
    }
}

void AST::dumpDeclarations(const Declarations& decls, int indent)
{
    for (auto &item : decls.items) {
        std::cout << indentStr(indent) << "Decl: " << item.name;
        if (item.isArray) {
            std::cout << " [" << item.rangeStart << ":" << item.rangeEnd << "]";
        }
        std::cout << "\n";

        // If the DeclItem has a symbol pointer, print symbol details
        if (item.symbol) {
            dumpSymbol(item.symbol, indent + 2);
        }
    }
}

void AST::dumpCommand(const Command& cmd, int indent)
{
    switch (cmd.type) {
    case CommandType::ASSIGN: {
        std::cout << indentStr(indent) << "Assign:\n";
        if (cmd.assignCmd) {
            std::cout << indentStr(indent + 2) << "LHS: ";
            dumpIdentifier(cmd.assignCmd->lhs, 0);

            std::cout << indentStr(indent + 2) << "RHS:\n";
            dumpExpression(*cmd.assignCmd->rhs, indent + 4);
        }
        break;
    }
    case CommandType::IF_THEN:
    case CommandType::IF_THEN_ELSE: {
        std::cout << indentStr(indent) << "If:\n";
        if (cmd.ifCmd) {
            std::cout << indentStr(indent + 2) << "Condition:\n";
            dumpCondition(*cmd.ifCmd->condition, indent + 4);

            if (!cmd.ifCmd->thenCommands.empty()) {
                std::cout << indentStr(indent + 2) << "Then:\n";
                for (auto &c : cmd.ifCmd->thenCommands) {
                    dumpCommand(*c, indent + 4);
                }
            }
            if (!cmd.ifCmd->elseCommands.empty()) {
                std::cout << indentStr(indent + 2) << "Else:\n";
                for (auto &c : cmd.ifCmd->elseCommands) {
                    dumpCommand(*c, indent + 4);
                }
            }
        }
        break;
    }
    case CommandType::WHILE: {
        std::cout << indentStr(indent) << "While:\n";
        if (cmd.whileCmd) {
            std::cout << indentStr(indent + 2) << "Condition:\n";
            dumpCondition(*cmd.whileCmd->condition, indent + 4);
            if (!cmd.whileCmd->body.empty()) {
                std::cout << indentStr(indent + 2) << "Body:\n";
                for (auto &c : cmd.whileCmd->body) {
                    dumpCommand(*c, indent + 4);
                }
            }
        }
        break;
    }
    case CommandType::REPEAT: {
        std::cout << indentStr(indent) << "Repeat:\n";
        if (cmd.repeatCmd) {
            if (!cmd.repeatCmd->body.empty()) {
                std::cout << indentStr(indent + 2) << "Body:\n";
                for (auto &c : cmd.repeatCmd->body) {
                    dumpCommand(*c, indent + 4);
                }
            }
            std::cout << indentStr(indent + 2) << "Until Condition:\n";
            dumpCondition(*cmd.repeatCmd->condition, indent + 4);
        }
        break;
    }
    case CommandType::FOR_TO:
    case CommandType::FOR_DOWNTO: {
        std::cout << indentStr(indent)
                  << (cmd.type == CommandType::FOR_TO
                      ? "ForTo:\n"
                      : "ForDownTo:\n");
        if (cmd.forCmd) {
            std::cout << indentStr(indent + 2)
                      << "Iterator: " << cmd.forCmd->loopVar << "\n";
            // If there's a loopVarSymbol, we can show that too:
            if (cmd.forCmd->loopVarSymbol) {
                dumpSymbol(cmd.forCmd->loopVarSymbol, indent + 4);
            }

            std::cout << indentStr(indent + 2) << "Start:\n";
            dumpValue(*cmd.forCmd->startValue, indent + 4);
            std::cout << indentStr(indent + 2) << "End:\n";
            dumpValue(*cmd.forCmd->endValue, indent + 4);

            if (!cmd.forCmd->body.empty()) {
                std::cout << indentStr(indent + 2) << "Body:\n";
                for (auto &c : cmd.forCmd->body) {
                    dumpCommand(*c, indent + 4);
                }
            }
        }
        break;
    }
    case CommandType::PROC_CALL: {
        std::cout << indentStr(indent) << "ProcCall:\n";
        if (cmd.procCallCmd) {
            std::cout << indentStr(indent + 2)
                      << "Name: " << cmd.procCallCmd->name << "\n";
            if (!cmd.procCallCmd->args.empty()) {
                std::cout << indentStr(indent + 2) << "Args:\n";
                for (auto &a : cmd.procCallCmd->args) {
                    std::cout << indentStr(indent + 4) << a << "\n";
                }
            }
        }
        break;
    }
    case CommandType::READ: {
        std::cout << indentStr(indent) << "Read:\n";
        if (cmd.ioCmd && cmd.ioCmd->readTarget) {
            std::cout << indentStr(indent + 2) << "Target: ";
            dumpIdentifier(*cmd.ioCmd->readTarget, 0);
        }
        break;
    }
    case CommandType::WRITE: {
        std::cout << indentStr(indent) << "Write:\n";
        if (cmd.ioCmd && cmd.ioCmd->writeValue) {
            dumpValue(**cmd.ioCmd->writeValue, indent + 2);
        }
        break;
    }
    }
}

void AST::dumpExpression(const Expression& expr, int indent)
{
    std::cout << indentStr(indent) << "Expression: ";
    switch (expr.op) {
    case ExprOp::NONE:    std::cout << "(NONE)\n";    break;
    case ExprOp::PLUS:    std::cout << "(+)\n";       break;
    case ExprOp::MINUS:   std::cout << "(-)\n";       break;
    case ExprOp::STAR:    std::cout << "(*)\n";       break;
    case ExprOp::SLASH:   std::cout << "(/)\n";       break;
    case ExprOp::PERCENT: std::cout << "(%)\n";       break;
    }

    if (expr.left) {
        std::cout << indentStr(indent + 2) << "Left:\n";
        dumpValue(*expr.left, indent + 4);
    }
    if (expr.op != ExprOp::NONE && expr.right) {
        std::cout << indentStr(indent + 2) << "Right:\n";
        dumpValue(*expr.right, indent + 4);
    }
}

void AST::dumpCondition(const Condition& cond, int indent)
{
    std::cout << indentStr(indent) << "Condition: ";
    switch (cond.op) {
    case CondOp::EQ:  std::cout << "EQ\n";  break;
    case CondOp::NEQ: std::cout << "NEQ\n"; break;
    case CondOp::GT:  std::cout << "GT\n";  break;
    case CondOp::LT:  std::cout << "LT\n";  break;
    case CondOp::GEQ: std::cout << "GEQ\n"; break;
    case CondOp::LEQ: std::cout << "LEQ\n"; break;
    }
    std::cout << indentStr(indent + 2) << "Left:\n";
    dumpValue(*cond.left, indent + 4);
    std::cout << indentStr(indent + 2) << "Right:\n";
    dumpValue(*cond.right, indent + 4);
}

void AST::dumpValue(const Value& val, int indent)
{
    if (val.isIdentifier) {
        std::cout << indentStr(indent) << "Value: Identifier\n";
        dumpIdentifier(val.identifier, indent + 2);
    } else {
        std::cout << indentStr(indent) << "Value: Number " 
                  << val.numberValue << "\n";
    }
}

void AST::dumpIdentifier(const Identifier& id, int indent)
{
    std::cout << indentStr(indent) << id.name;
    switch (id.idxType) {
    case IdentifierIndexType::NONE:
        std::cout << " (no index)";
        break;
    case IdentifierIndexType::NUMERIC:
        std::cout << "[" << id.indexNumber << "]";
        break;
    case IdentifierIndexType::VARIABLE:
        std::cout << "[" << id.indexVar << "]";
        break;
    }
    std::cout << "\n";

    // If there's a symbol pointer, show it
    if (id.symbol) {
        dumpSymbol(id.symbol, indent + 2);
    }
}

/**
 * \brief A helper that prints out a Symbol's details,
 *        including its reference count.
 */
static void dumpSymbol(const std::shared_ptr<AST::Symbol> &sym, int indent)
{
    if (!sym) {
        std::cout << AST::indentStr(indent) << "Symbol: (null)\n";
        return;
    }

    // The shared_ptr reference count is in sym.use_count()
    std::cout << AST::indentStr(indent)
              << "Symbol: " << sym->name
              << " (refCount=" << sym.use_count() << ")\n";

    std::cout << AST::indentStr(indent + 2)
              << "isProcedure=" << sym->isProcedure
              << " isArray="     << sym->isArray
              << " isRefParam="  << sym->isRefParam
              << " isIterator="  << sym->isIterator << "\n";

    std::cout << AST::indentStr(indent + 2)
              << "arrayStart="   << sym->arrayStart
              << " arrayEnd="    << sym->arrayEnd
              << " address="     << sym->address
              << " isInitialized=" << sym->isInitialized << "\n";
}

} // namespace Compiler
