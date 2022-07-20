//
// Created by 김민성 on 2020/09/24.
//
#ifndef REALROMIO_ROMIO_H
#define REALROMIO_ROMIO_H
#include <string>
#include <iostream>
#include <ctype.h>
#include <map>
#include <vector>
#include <cctype>
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
using namespace std;
using namespace llvm;
LLVMContext theContext;
Module *module = new Module("MyModule", theContext);

typedef enum KEY_TOKEN{
    EOF_TOKEN = -1,
    FUNC_TOKEN = -2,
    IDENTIFIER_TOKEN = -3,
    NUMERIC_TOKEN = -4,

    IF_TOKEN = -5,
    ELSE_TOKEN = -6,
    ELSE_IF_TOKEN = -7,
    FOR_TOKEN = -8,
    IN_TOKEN = -9,

    UNARY_TOKEN = -10,
    BINARY_TOKEN = -11,
    END_TOKEN = -12,
    MAIN_TOKEN = -13,
}KEY_TOKEN;

namespace ADT{
    class AST{
    public:
        virtual ~AST(){}
        virtual Value *Codegen() = 0;
    };

    class NumericAST : public AST{
        int numeric_value;
    public:
        NumericAST(int val):numeric_value(val){};
        Value *Codegen() override;
    };


    class VarAST : public AST{
        std::string varName;
    public:
        VarAST(const std::string &name) : varName(name){}
        Value *Codegen() override;
    };


    class UnaryAST : public AST{
        char crtOp;
        AST * Operand;
    public:
        UnaryAST(char op, AST *operand):crtOp(op),Operand(operand){}
        Value *Codegen() override;
    };


    class BinaryAST: public AST{
        std::string binOp;
        AST *lh, *rh;
    public:
        BinaryAST(const std::string op, AST *LH, AST *RH):binOp(op),lh(LH),rh(RH){}
        Value *Codegen() override;
    };


    class FunctionCallAST : public AST{
        std::string function_callee;
        std::vector<AST *> function_arguments;
    public:
        FunctionCallAST(const std::string &callee, std::vector<AST *> &args)
                    :function_callee(callee),function_arguments(args){}
        Value *Codegen() override;
    };



    class ExpIfAST: public AST{
        AST *Cond, *Else, *Elseif;
    public:
        ExpIfAST(AST *cond, AST *elsest, AST *elseif)
                    :Cond(cond), Else(elsest), Elseif(elseif){}
        Value *Codegen() override;
    };



    class ExprForAST: public AST{
        std::string condVarName;
        AST *Start, *End, *Step, *Body;
    public:
        ExprForAST(const std::string &name, AST *start, AST *end,
                   AST *step, AST *body)
                   :condVarName(name),Start(start),End(end),Step(step),Body(body){}
        Value *Codegen() override;
    };

    class FunctionDeclAST{
        std::string funcName;
        std::vector<std::string> arguments;
        bool isOp; unsigned precedence;
    public:
        FunctionDeclAST(const std::string &name, const std::vector<std::string> &args,
                        bool isOperator = false, unsigned prec = 0)
                        :funcName(name),arguments(args),isOp(isOperator),precedence(prec){}
        bool isUnaryOp() const {return isOp && arguments.size() == 1;}
        bool isBinaryOp() const {return isOp && arguments.size() == 2;}
        char getOpName() const{
            assert(isUnaryOp() || isBinaryOp());
            return funcName[funcName.size() - 1];
        }
        unsigned getBinaryPrecedence() const {return precedence;}
        Function *Codegen();
    };


    class FunctionDefnAST{
        FunctionDeclAST *funcDecl;
        AST *body;
    public:
        FunctionDefnAST(FunctionDeclAST *first, AST *body):funcDecl(first),
        body(body){}
        Function *Codegen();
    };
}
#endif //REALROMIO_ROMIO_H
