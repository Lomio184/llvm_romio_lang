#include <iostream>
#include <string>
#include <ctype.h>
#include <map>
#include <vector>
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
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

FILE *file;

static int Numeric_val;
static std::string current_Identifier_string;
static void handleDefinition();
static int get_token(){
    static int finalChar = ' ';

    while(isspace(finalChar)) finalChar = fgetc(file);

    if(isalpha(finalChar)){
        current_Identifier_string = finalChar;
        while(isalnum(finalChar = fgetc(file)))
            current_Identifier_string += finalChar;

        if(current_Identifier_string == "func") return FUNC_TOKEN;
        if(current_Identifier_string == "if")   return IF_TOKEN;
        if(current_Identifier_string == "else") return ELSE_TOKEN;
        if(current_Identifier_string == "elif") return ELSE_IF_TOKEN;
        if(current_Identifier_string == "for")  return FOR_TOKEN;
        if(current_Identifier_string == "in")   return IN_TOKEN;
        if(current_Identifier_string == "unary")return UNARY_TOKEN;
        if(current_Identifier_string == "end") return END_TOKEN;
        return IDENTIFIER_TOKEN;
    }
    if(isdigit(finalChar) || finalChar == '.'){
        std::string numericValue;
        do{
            numericValue += finalChar;
            finalChar = fgetc(file);
        } while (isdigit(finalChar) || finalChar == '.');
        numericValue = std::stoi(numericValue.c_str(), 0);
        return NUMERIC_TOKEN;
    }
    if(finalChar == '#'){
        do {
            finalChar = fgetc(file);
        }while(finalChar != EOF && finalChar != '\n' && finalChar != '\r');

        if(finalChar != EOF)
            return get_token();
    }
    if(finalChar == EOF)
        return EOF_TOKEN;
    int thisChar = finalChar;
    finalChar = fgetc(file);
    return thisChar;
}

static int current_token;

static int next_token(){ return current_token = get_token(); }

static std::map<char, int> operator_precedence;

static int getBinOpPrecedence(){
    if(!isascii(current_token))
        return -1;
    int operator_prec = operator_precedence[current_token];
    if(operator_prec <= 0)
        return -1;
    return operator_prec;
}

static ADT::AST *expression_parser();

static ADT::AST *identifier_parser(){
    std::string idName = current_Identifier_string;
    get_token();
    if (current_token != '(')
        return new ADT::VarAST(idName);

    get_token();
    std::vector<ADT::AST *> arguments;
    if (current_token != ')'){
        while (1){
            ADT::AST *arg = expression_parser();
            if (!arg) return 0;
            arguments.push_back(arg);

            if (current_token == ')') break;

            if (current_token != ',') return 0;
            get_token();
        }
    }
    get_token();

    return new ADT::FunctionCallAST(idName, arguments);
}

static ADT::AST * numeric_parser(){
    ADT::AST *result = new ADT::NumericAST(Numeric_val);
    get_token();
    return result;
}

static ADT::AST *paran_parser(){
    get_token();
    ADT::AST * rea = expression_parser();
    if(!rea) return 0;
    if(current_token != ')') return 0;
    get_token();
    return rea;
}

static ADT::AST * if_parser(){
    get_token();
    ADT::AST * condition = expression_parser();

    if(!condition) return 0;

    get_token();
    if(current_token != ELSE_IF_TOKEN) return 0;

    ADT::AST *ElseIf = expression_parser();
    if(ElseIf == 0) return 0;

    if(current_token != ELSE_TOKEN) return 0;
    get_token();

    ADT::AST *Else = expression_parser();
    if(!Else) return 0;
    return new ADT::ExpIfAST(condition, ElseIf, Else);
}

static ADT::AST *for_parser(){
    get_token();
    if(current_token != IDENTIFIER_TOKEN) return 0;

    std::string idName = current_Identifier_string;
    get_token();
}

static ADT::AST *base_parser(){
    switch(current_token){
        default:
            return 0;
        case IDENTIFIER_TOKEN:
            return identifier_parser();
        case NUMERIC_TOKEN:
            return numeric_parser();
        case '(':
            return paran_parser();
        case IF_TOKEN:
            return if_parser();
    }
}

static ADT::AST * unary_parser(){
    if(!isascii(current_token) || current_token == '(' || current_token == ',')
        return base_parser();
    int op = current_token;
    get_token();
    if(ADT::AST *operand = unary_parser())
        return new ADT::UnaryAST(op, operand);
    return 0;
}

static ADT::AST *binary_op_parser(int prec, ADT::AST *lhs){
    while (1){
        int op_prec = getBinOpPrecedence();
        if(op_prec < prec) return lhs;

        int bin_Op = current_token;
        get_token();

        ADT::AST *rhs = unary_parser();
        if(!rhs) return 0;

        int nextPrec = getBinOpPrecedence();
        if(op_prec < nextPrec){
            rhs = binary_op_parser(op_prec + 1, rhs);
            if(rhs == 0) return 0;
        }
        lhs = new ADT::BinaryAST(std::to_string(bin_Op), lhs, rhs);
    }
}

static ADT::AST *expression_parser(){
    ADT::AST *lhs = unary_parser();
    if(!lhs) return 0;
    return binary_op_parser(0, lhs);
}

static ADT::FunctionDeclAST * func_decl_parser(){
    std::string functionName;

    unsigned kind = 0;
    unsigned binaryPrecedence = 30;

    switch (current_token) {
        default: return 0;
        case IDENTIFIER_TOKEN:
            functionName = current_Identifier_string;
            kind = 0;
            get_token();
            break;
        case UNARY_TOKEN:
            get_token();
            if(!isascii(current_token)) return 0;
            functionName = "unary";
            functionName += (char)current_token;
            kind = 1;
            get_token();
            break;
        case BINARY_TOKEN:
            get_token();
            if(!isascii(current_token)) return 0;
            functionName = "binary";
            functionName += (char)current_token;
            kind = 2;
            get_token();

            if(current_token == NUMERIC_TOKEN){
                if(Numeric_val < 1 || Numeric_val > 100) return 0;
                binaryPrecedence = (unsigned)Numeric_val;
                get_token();
            }
            break;
    }

    if(current_token != '(') return 0;
    std::vector<std::string> argNames;
    while(get_token() == IDENTIFIER_TOKEN)
        argNames.push_back(current_Identifier_string);
    if(current_token != ')')
        return 0;
    get_token();

    if(kind && argNames.size() != kind) return 0;

    return new ADT::FunctionDeclAST(functionName, argNames, kind != 0, binaryPrecedence);
}

static ADT::FunctionDefnAST *func_defn_parser(){
    get_token();
    ADT::FunctionDeclAST *funcDeclParser = func_decl_parser();
    if(funcDeclParser == 0)
        return 0;
    if(ADT::AST *E = expression_parser())
        return new ADT::FunctionDefnAST(funcDeclParser, E);
    return 0;
}

static ADT::FunctionDefnAST *top_level_parser(){
    if(ADT::AST *E = expression_parser()){
        ADT::FunctionDeclAST *funcDeclParser = new ADT::FunctionDeclAST("",
                                                                        std::vector<std::string>());
        return new ADT::FunctionDefnAST(funcDeclParser, E);
    }
    return 0;
}

static Module *module_Ob;
static IRBuilder<> Builder(theContext);
static std::map<std::string, Value*> named_Values;
static legacy::FunctionPassManager *Global_fp;

Value *ADT::NumericAST::Codegen(){
    return ConstantInt::get(Type::getInt32Ty(theContext), numeric_value);
}

Value *ADT::VarAST::Codegen() {
    Value *V = named_Values[varName];
    return V ? V : 0;
}

Value* ADT::UnaryAST::Codegen() {
    Value *operandV = Operand -> Codegen();
    if(operandV == 0) return 0;

    Function *F = module_Ob -> getFunction(std::string("unary") +
                                           crtOp);
    if(F == 0) return 0;
    return Builder.CreateCall(F, operandV, "unop");
}

Value *ADT::BinaryAST::Codegen() {
    Value *lvalue = lh -> Codegen();
    Value *rvalue = rh -> Codegen();
    if(lvalue == 0 || rvalue == 0)
        return 0;

    switch(std::stoi(binOp)){
        case '+':
            return Builder.CreateAdd(lvalue, rvalue, "addRomio");
        case '-':
            return Builder.CreateSub(lvalue, rvalue, "sumRomio");
        case '*':
            return Builder.CreateMul(lvalue, rvalue, "mulRomio");
        case '/':
            return Builder.CreateFDiv(lvalue, rvalue, "divRomio");
        case '<':
            lvalue = Builder.CreateICmpULT(lvalue, rvalue, "cmpRomio");
            return Builder.CreateZExt(lvalue, Type::getInt32Ty(theContext),
                                      "boolRomio");
        default: break;
    }

    Function *F = module_Ob -> getFunction(std::string("binary") + binOp);
    assert(F && "binary operator not found!");

    Value *ops[] = {lvalue, rvalue};
    return Builder.CreateCall(F, ops, "binop");
}

Value * ADT::FunctionCallAST::Codegen() {
    Function *calleeF = module_Ob -> getFunction(function_callee);
    if(calleeF == 0) return 0;
    if(calleeF -> arg_size() != function_arguments.size()) return 0;

    std::vector<Value *> argsV;

    for(unsigned i = 0, e = function_arguments.size(); i != e; ++i){
        argsV.push_back(function_arguments[i] -> Codegen());
        if(argsV.back() == 0) return 0;
    }
    return Builder.CreateCall(calleeF, argsV, "calltmp");
}

Value * ADT::ExpIfAST::Codegen() {
    Value *condV = Cond -> Codegen();
    if(condV == 0) return 0;

    condV = Builder.CreateICmpNE(
            condV, ConstantInt::get(Type::getInt32Ty(theContext), 0,"ifcond")
    );
    Function *theFunction = Builder.GetInsertBlock() -> getParent();

    BasicBlock *ElseIfBB=
            BasicBlock::Create(theContext, "elseif", theFunction);
    BasicBlock *ElseBB =
            BasicBlock::Create(theContext, "else");
    BasicBlock *MergeBB =
            BasicBlock::Create(theContext, "ifcont");

    Builder.CreateCondBr(condV, ElseIfBB, ElseBB);
    Builder.SetInsertPoint(ElseIfBB);

    Value *ElseIfV = Elseif -> Codegen();
    if(ElseIfV == 0) return 0;
    Builder.CreateBr(MergeBB);
    ElseIfBB = Builder.GetInsertBlock();
    theFunction -> getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);

    Value * elseV = Else -> Codegen();
    if(elseV == 0) return 0;
    Builder.CreateBr(MergeBB);
    ElseBB = Builder.GetInsertBlock();
    theFunction -> getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);

    PHINode *PN =
            Builder.CreatePHI(Type::getInt32Ty(theContext), 2, "iftmp");
    PN -> addIncoming(ElseIfV, ElseIfBB);
    PN -> addIncoming(elseV, ElseBB);
    return PN;
}
/*redeclaration this section// fix this language design issue */
Value *ADT::ExprForAST::Codegen() {
    Value *startVal = Start -> Codegen();
}

Function *ADT::FunctionDeclAST::Codegen() {
    std::vector<Type *> intergers(arguments.size(),
                                  Type::getInt32Ty(theContext));
    FunctionType *FT =
            FunctionType::get(Type::getInt32Ty(theContext),intergers, false);
    Function *F =
            Function::Create(FT, Function::ExternalLinkage, funcName, module_Ob);

    if(F -> getName() != funcName){
        F -> eraseFromParent();
        F = module_Ob -> getFunction(funcName);
        if(!F -> empty()) return 0;

        if(F -> arg_size() != arguments.size()) return 0;
    }
    unsigned idx = 0;
    for(Function::arg_iterator AI = F -> arg_begin(); idx != arguments.size();
        ++AI, ++idx){
        AI -> setName(arguments[idx]);
        named_Values[arguments[idx]] = AI;
    }
    return F;
}

Function *ADT::FunctionDefnAST::Codegen() {
    named_Values.clear();
    Function *theFunction = funcDecl -> Codegen();
    if(theFunction == 0) return 0;

    if(funcDecl -> isBinaryOp())
        operator_precedence[funcDecl -> ADT::FunctionDeclAST::getOpName()] =
                funcDecl -> ADT::FunctionDeclAST::getBinaryPrecedence();
    BasicBlock *BB = BasicBlock::Create(theContext, "entry", theFunction);
    Builder.SetInsertPoint(BB);

    if(Value * retVal = body -> Codegen()){
        Builder.CreateRet(retVal);
        verifyFunction(*theFunction);

        Global_fp -> run(*theFunction);
        return theFunction;
    }
    theFunction -> eraseFromParent();
    if(funcDecl ->ADT::FunctionDeclAST::isBinaryOp())
        operator_precedence.erase(funcDecl -> ADT::FunctionDeclAST::getOpName());
    return 0;

}

static ExecutionEngine *theExecutionEngine;

static void handleDefinition(){
    if(ADT::FunctionDefnAST *F = func_defn_parser()){
        if(Function *LF = F -> Codegen()){

        }else{
            next_token();
        }
    }
}

static void handleTopLevelExpr(){
    if(ADT::FunctionDefnAST *F = top_level_parser()){
        if(Function *LF = F -> Codegen()){
            theExecutionEngine -> finalizeObject();
            void *fPtr = theExecutionEngine -> getPointerToFunction(LF);

            double (*FP)() = (double(*)())(intptr_t)fPtr;
            fprintf(stderr, "Evaluate to %f\n", FP());
        }
    }else next_token();
}

extern "C" double putchard(double X){
    putchar((char)X);
    return 0;
}

extern "C" double printd(double X){
    printf("%f\n", X);
    return 0;
}

static void driver(){
    while(1){
        switch (current_token) {
            case EOF_TOKEN:
                return;
            case ';':
                next_token();
                break;
            case FUNC_TOKEN:
                handleDefinition();
                break;
            default:
                handleTopLevelExpr();
                break;
        }
    }
}




static void init_precedence(){
    operator_precedence['<'] = 10;
    operator_precedence['-'] = 20;
    operator_precedence['+'] = 30;
    operator_precedence['/'] = 40;
    operator_precedence['*'] = 50;
}


int main(int argc, char *argv[]) {
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    init_precedence();

    file = fopen(argv[1], "r");
    if(file == 0) printf("Could not open! \n");
    next_token();
    std::unique_ptr<Module> Owner = std::make_unique<Module>("Romio Engine Start", theContext);
    module_Ob = Owner.get();

    std::string errStr;
    theExecutionEngine =
            EngineBuilder(std::move(Owner))
                .setErrorStr(&errStr)
                .setMCJITMemoryManager(std::make_unique<SectionMemoryManager>())
                .create();
    if(!theExecutionEngine)
        exit(1);

    legacy::FunctionPassManager ourFpm(module_Ob);

    module_Ob -> setDataLayout(theExecutionEngine -> getDataLayout());
    ourFpm.add(createCostModelAnalysisPass());
    ourFpm.add(createReassociatePass());
    ourFpm.add(createNewGVNPass());
    ourFpm.add(createCFGSimplificationPass());

    ourFpm.doInitialization();
    Global_fp = &ourFpm;

    driver();

    Global_fp = 0;
    module_Ob -> print(llvm::errs(), 0);
    return 0;
}
