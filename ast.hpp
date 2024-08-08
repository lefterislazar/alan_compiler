//TODO CHECK ALL FUNCTIONS HAVE RETURN STATEMENTS

#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <string.h>
#include <vector>
#include <map>
#include <list>
#include <utility>



#include <llvm/Pass.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>


#include "symbol.hpp"

typedef llvm::Value Value;


class AST {
	public:
		virtual void printAST(std::ostream &out) const = 0;
		virtual ~AST() = default;
		virtual void sem(){};
		virtual void compile_and_dump(bool optimize=true)  { 
			// Initialize
			TheModule = std::make_unique<llvm::Module>("alan program", TheContext);
			TheFPM = std::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());
			if (optimize) {
				TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
				TheFPM->add(llvm::createInstructionCombiningPass());
				TheFPM->add(llvm::createReassociatePass());
				TheFPM->add(llvm::createGVNPass());
				TheFPM->add(llvm::createCFGSimplificationPass());
			}
			TheFPM->doInitialization();

			// Initialize types
			// llvm::Type* i1  = llvm::IntegerType::get(TheContext, 1);
			i8  = llvm::IntegerType::get(TheContext, 8);
			i16  = llvm::IntegerType::get(TheContext, 16);
			i32 = llvm::IntegerType::get(TheContext, 32);
			// llvm::Type* i64 = llvm::IntegerType::get(TheContext, 64);

			// llvm::ArrayType* vars_type = llvm::ArrayType::get(i32, 26);
			llvm::ArrayType* nl_type = llvm::ArrayType::get(i8, 2);

			// Initialize global variables
			llvm::GlobalVariable* TheNL = new llvm::GlobalVariable(
				*TheModule, nl_type, true, llvm::GlobalValue::PrivateLinkage,
				llvm::ConstantArray::get(nl_type, {llvm::ConstantInt::get(i8, '\n'), llvm::ConstantInt::get(i8, '\0')}), "nl");
			TheNL->setAlignment(llvm::MaybeAlign(1));

			// Initialize library functions
			// llvm::FunctionType* writeInteger_type = 
			// 	llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i64}, false);
			// llvm::Function* TheWriteInteger = 
			// 	llvm::Function::Create(writeInteger_type, llvm::Function::ExternalLinkage,
			// 						"writeInteger", TheModule.get());

			// llvm::FunctionType* writeString_type = 
			// 	llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext),
			// 							{llvm::PointerType::get(i8, 0)}, false);
			// llvm::Function* TheWriteString = 
			// 	llvm::Function::Create(writeString_type, llvm::Function::ExternalLinkage,
			// 						"writeString", TheModule.get());


            std::string id="writeInteger";
            std::vector<Types> types = {Type_int}; 
            createLibraryFunction(&id,types,Proc);
            id="writeByte";
            types = {Type_byte}; 
            createLibraryFunction(&id,types,Proc);
            id="writeChar";
            types = {Type_byte}; 
            createLibraryFunction(&id,types,Proc);
            id="writeString";
            types = {Type_byte_array}; 
            createLibraryFunction(&id,types,Proc);


            id="readInteger";
            types = {}; 
            createLibraryFunction(&id,types,rtype_int);
            id="readByte";
            types = {}; 
            createLibraryFunction(&id,types,rtype_byte);
            id="readChar";
            types = {}; 
            createLibraryFunction(&id,types,rtype_byte);
            id="readString";
            types = {Type_int,Type_byte_array}; 
            createLibraryFunction(&id,types,Proc);

            id="extend";
            types = {Type_byte}; 
            createLibraryFunction(&id,types,rtype_int);
            id="shrink";
            types = {Type_int}; 
            createLibraryFunction(&id,types,rtype_byte);


            id="strlen";
            types = {Type_byte_array}; 
            createLibraryFunction(&id,types,rtype_int);
            id="strcmp";
            types = {Type_byte_array,Type_byte_array}; 
            createLibraryFunction(&id,types,rtype_int);
            id="strcpy";
            types = {Type_byte_array,Type_byte_array}; 
            createLibraryFunction(&id,types,Proc);
            id="strcat";
            types = {Type_byte_array,Type_byte_array}; 
            createLibraryFunction(&id,types,Proc);





			// Define and start the main function.
			
			// llvm::FunctionType* main_type = llvm::FunctionType::get(i32, {}, false);
			// llvm::Function* main = llvm::Function::Create(main_type, llvm::Function::ExternalLinkage,
			// 											"main", TheModule.get());
			// llvm::BasicBlock* BB = llvm::BasicBlock::Create(TheContext, "entry", main);
			// Builder.SetInsertPoint(BB);

			// Emit the program code
			// Assume `igen()` is a function that generates code
			// Replace with your actual implementation
			// igen(); 
			my_main=false;

			llvm::Function* main=compile_main();

			Builder.CreateRet(llvm::ConstantInt::get(i32, 0));

			// Verify the IR.
			bool bad = llvm::verifyModule(*TheModule, &llvm::errs());
			if (bad) {
				std::cerr << "The IR is bad!" << std::endl;
				TheModule->print(llvm::errs(), nullptr);
				std::exit(1);
			}

			// Optimize!
			TheFPM->run(*main);

			// Print out the IR.
			TheModule->print(llvm::outs(), nullptr);


		}
		virtual llvm::Function * compile_main()  {return nullptr;}
		virtual llvm::Value * compile()  {return nullptr;}

		void set_line(int lineno){
			line=lineno;
		}
		static void createLibraryFunction(std::string * id, std::vector<Types> argTypes, Rtypes returnType) {
			std::vector<llvm::Type*> llvmArgTypes;
			for (Types type : argTypes) {
				switch (type) {
					case Type_int:     llvmArgTypes.push_back(llvm::Type::getInt16Ty(TheContext)); break;
					case Type_byte:    llvmArgTypes.push_back(llvm::Type::getInt8Ty(TheContext)); break;
					case Type_bool:    llvmArgTypes.push_back(llvm::Type::getInt1Ty(TheContext)); break;
					case Type_int_array: llvmArgTypes.push_back(llvm::PointerType::get(i16, 0)); break;
					case Type_byte_array: llvmArgTypes.push_back(llvm::PointerType::get(i8, 0)); break;
				}
			}

			llvm::Type* llvmReturnType;
			switch (returnType) {
				case rtype_int:     llvmReturnType = llvm::Type::getInt16Ty(TheContext); break;
				case rtype_byte:    llvmReturnType = llvm::Type::getInt8Ty(TheContext); break;
				case Proc:    llvmReturnType = llvm::Type::getVoidTy(TheContext); break;
			}

			llvm::FunctionType* funcType = llvm::FunctionType::get(llvmReturnType, llvmArgTypes, false);
			llvm::Function* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, *id, TheModule.get());
			IRSTFun.insert(id,argTypes,returnType,function);
			
		}
	protected:
		int line;
		static bool my_main;
		static llvm::LLVMContext TheContext;
		static llvm::IRBuilder<> Builder;
		static std::unique_ptr<llvm::Module> TheModule;
		static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;

		static llvm::GlobalVariable *TheVars;
		static llvm::GlobalVariable *TheNL;
		static llvm::Function *TheWriteInteger;
		static llvm::Function *TheWriteString;

		static llvm::Type *i8;
		static llvm::Type *i16;
		static llvm::Type *i32;
		static llvm::Type *i64;

		static llvm::ArrayType *vars_type;
		static llvm::ArrayType *nl_type;

		static llvm::ConstantInt* c8(char c) {
			return llvm::ConstantInt::get(TheContext, llvm::APInt(8, c, true));
		}
		static llvm::ConstantInt* c1(int n) {
			return llvm::ConstantInt::get(TheContext, llvm::APInt(1, n, true));
		}
		static llvm::ConstantInt* c16(int n) {
			return llvm::ConstantInt::get(TheContext, llvm::APInt(16, n, true));
		}
};



inline std::ostream &operator<<(std::ostream &out, Datatype t) {
	switch(t) {
		case DATATYPE_int:  out << "int";  break;
		case DATATYPE_byte: out << "byte"; break;
	}	
	return out;
}

inline std::ostream &operator<<(std::ostream &out, Types t) {
	switch(t) {
		case Type_byte:  out << "byte";  break;
		case Type_int: out << "int"; break;
		case Type_byte_array:  out << "byte array";  break;
		case Type_int_array: out << "int array"; break;
		case Type_bool: out << "bool"; break;

	}	
	return out;
}


inline std::ostream &operator<<(std::ostream &out, Rtypes t) {
	switch(t) {
		case rtype_int:  out << "int";  break;
		case rtype_byte: out << "byte"; break;
		case Proc: out << "proc"; break;
	}	
	return out;
}


inline std::ostream &operator<<(std::ostream &out, const AST &ast) {
  	ast.printAST(out);
  	return out;
}



class Type : public AST {
	public:
		Type(Datatype dt, bool isArray): datatype(dt), isArray(isArray) {};
		void printAST(std::ostream &out) const override {
			out << "Type(" << datatype << ",";
			if(isArray)
				out << "Array";
			else
				out << "Scalar";
			out << ")";
		}
		void sem() override{
			if(isArray){
				if(datatype==DATATYPE_byte){
					type = Type_byte_array;
				}
				else{
					type = Type_int_array;	
				}
			}
			else{
				if(datatype== DATATYPE_byte){
					type = Type_byte;
				}
				else{
					type = Type_int;
				}
				
			}
		}
		Types get_type(){
			return type;
		}
	private:
		Datatype datatype;
		bool isArray;
		Types type;
};

class RType : public AST {
	public: 
		RType(bool isProc, Datatype dt = DATATYPE_int): isProc(isProc), datatype(dt) {}
		void printAST(std::ostream &out) const override {
			out << "RType(";
			if(isProc)
				out << "proc";
			else
				out << datatype;
			out << ")";
		}
		Rtypes get_rtype(){
			Rtypes rtype;
			if(isProc){
				rtype=Proc;
			}
			else{
				if(datatype==DATATYPE_int){
					rtype= rtype_int;
				}
				else{
					rtype=rtype_byte;
				}
			}
			return rtype;
		}
		private:
			bool isProc;
			Datatype datatype;
		};


class FParDef : public AST {
	public:
		FParDef(std::string *var, Type t, bool isRef):  var(var), type(t), isRef(isRef) {}

		void printAST(std::ostream &out) const override {
			out << "FParDef(" << *var << "," << (isRef ? "reference " : "") << type << ")";
		}
		bool get_ref(){
			return isRef;
		}
		void sem() override{
			type.sem();
		}
		Types get_type(){
			return type.get_type();
		}
		void insert_param(){
			STVar.insert(var,type.get_type(),-1,line);
		}
		std::string * get_name(){
			return var;
		}
		// Value * compile() override{
		// 	IRSTVar.insert(var,type.get_type(),-1,)
		// }
	private:
		std::string *var;
		Type type;
		bool isRef;
};

class FParList : public AST {
	public:
		FParList() {}
		void append(FParDef * fpd) {
			pardef_list.push_back(fpd);
		}

		void printAST(std::ostream &out) const override {
			bool first = 1;
			out << "FParList(";
			for(const auto &it : pardef_list) {
				if(!first) {
					out << ",";
				} else 
					first = 0;
				out << *it;
			}
		}
		void sem() override{
			for(const auto &i : pardef_list){
				i->sem();

			}
		}
		void insert_params(){
			for(const auto &i : pardef_list){
				i->insert_param();
			}	
		}
		std::vector<Types> get_type_list(){
			std::vector<Types>  parameters;
			for(const auto &i : pardef_list){
				parameters.push_back(i->get_type());
			}
			return parameters;
		}
		std::vector<std::string *> get_name_list(){
			std::vector<std::string *>   parameters;
			for(const auto &i : pardef_list){
				parameters.push_back(i->get_name());
			}
			return parameters;		
		}

		// Value * compile(){
		// 	for(const auto &i : pardef_list){
		// 		i->compile();
		// 	}	
		// 	return nullptr;
		// }

	private:
		std::list<FParDef *> pardef_list;
};



//class Expr

class Expr : public AST{
	public:
		virtual Types get_type(){
				return type;
		}
	protected:
		Types type;
};

class Cond :public AST{};

class CondConst : public Cond{
	public:
		CondConst(bool cond):cond(cond){}
		void printAST(std::ostream &out) const override {
			out <<"CondConst(";
			if(cond)
				out<<"true";
			else
				out<<"false";
			out<<")";
		}	
		void sem() override{}
		Value * compile() override{
			if(cond)
				return c1(1);
			return c1(0);
		}
	private:
		bool cond;
};

class CompareCond : public Cond{
	public:
		CompareCond(Expr *expr1,Expr *expr2,char op):expr1(expr1),expr2(expr2),op(op){}
		void printAST(std::ostream &out) const override {
			out <<"CompareCond("<<*expr1<<","<<op<<","<<*expr2<<")";
		}	
		void sem() override{
			expr1->sem();
			expr2->sem();
			if(expr1->get_type()!=Type_int && expr1->get_type()!=Type_byte ){
				yyerror("Binary operators can only be applied to integers and bytes\n",line);
			}
			if(expr1->get_type()!=expr2->get_type()){
				yyerror("Operands must be of the same type\n",line);
			}

		}	
		Value * compile() override{
			Value* l = expr1->compile();
			Value* r = expr2->compile();
			switch(op) {
				case '>': 
					return Builder.CreateICmpSGT(l, r, "gttmp");  
				case '<': 
					return Builder.CreateICmpSLT(l, r, "lttmp");  
				case 'e': 
					return Builder.CreateICmpEQ(l, r, "eqtmp");   
				case 'i': 
					return Builder.CreateICmpNE(l, r, "netmp");  
				case 'l': 
					return Builder.CreateICmpSLE(l, r, "letmp");  
				case 'g': 
					return Builder.CreateICmpSGE(l, r, "getmp");  
			}
			return nullptr;
		}	
	private:
		Expr *expr1,*expr2;
		char op;
};

class CondOp : public Cond{
	public:
		CondOp(Cond *cond1,Cond *cond2,char op):cond1(cond1),cond2(cond2),op(op){}
		void printAST(std::ostream &out) const override {
			out <<"CompareCond("<<*cond1<<","<<op<<","<<*cond2<<")";
		}			
		void sem() override{
			cond1->sem();
			cond2->sem();
		}
		Value * compile() override{
			Value *l = cond1->compile();
			Value *r = cond2->compile();
			switch(op) {
				case '&': 
					return Builder.CreateAnd(l, r, "andtmp");
				case '|':  
					return Builder.CreateOr(l, r, "ortmp");
			}
			return nullptr;
		}
	private:
		Cond *cond1,*cond2;
		char op;
};

class NotCond : public Cond{
	public:
		NotCond(Cond *cond):cond(cond),op('!'){}
		void printAST(std::ostream &out) const override {
			out <<"NotCond("<<*cond<<",!"<<")";
		}	
		void sem() override{
			cond->sem();
		}		
		Value * compile() override{
			Value *val = cond->compile();
			return Builder.CreateNot(val,"nottmp");
		}
	private:
		Cond *cond;
		char op;
};


class ExprLitChar : public Expr{
	public:
		ExprLitChar(char var):var(var){}
		void printAST(std::ostream &out) const override {
			out <<"ExprLitChar("<<var<<")";
		}
		void sem() override{
			type = Type_byte; 
		}
		Value * compile() override{
			return c8(var);
		}
	private:
		char var;
};

class ExprLitInt : public Expr{
	public:
		ExprLitInt(int lit):lit(lit){}
		void printAST(std::ostream &out) const override {
			out <<"ExprLitInt("<<lit<<")";
		}
		void sem() override{
			type = Type_int; 
		}
		Value * compile() override{
			return c16(lit);
		}
	private:
		int lit;
};


class ExprUnitaryOp : public Expr{
	public:
		ExprUnitaryOp(Expr *expr,char op):expr(expr),op(op){}
		void printAST(std::ostream &out) const override {
			out <<"ExprUnitaryOp("<<*expr<<")";
		}	
		void sem() override{
			expr->sem();
			if(expr->get_type()!=Type_int){
				yyerror("+ or - can only be applied to integers\n",line);
			}
			type=Type_int; 
		}
		Value * compile() override{
			Value *val = expr->compile();
			if(op=='-'){
				return Builder.CreateSub(c16(0), val, "negtmp");
			}
			return val;  
		}
	private:
		Expr *expr;
		char op;
};

class ExprBinaryOp : public Expr{
	public:
		ExprBinaryOp(Expr *expr1,Expr *expr2,char op):expr1(expr1),expr2(expr2),op(op){}
		void printAST(std::ostream &out) const override {
			out <<"ExprBinaryOp("<<*expr1<<","<<*expr2<<")";
		}	
		void sem() override{
			expr1->sem();
			expr2->sem();
			if(expr1->get_type()!=Type_int && expr1->get_type()!=Type_byte ){
				yyerror("Binary operators can only be applied to integers and bytes",line);
			}
			if(expr1->get_type()!=expr2->get_type()){
				std::cout<<"Lhs is:"<<expr1->get_type()<<"\nRhs is:"<<expr2->get_type()<<std::endl;
				yyerror("Operands must be of the same type",line);
			}
			type = expr1->get_type();

			// TODO CHECK DIVISION BY 0 
		
		}
		Value * compile()override{
			Value* l = expr1->compile();
			Value* r = expr2->compile();
			switch(op) {
			case '+': return Builder.CreateAdd(l, r, "addtmp");
			case '-': return Builder.CreateSub(l, r, "subtmp");
			case '*': return Builder.CreateMul(l, r, "multmp");
			case '/': return Builder.CreateSDiv(l, r, "divtmp");
			case '%': return Builder.CreateSRem(l, r, "modtmp");
			}

			return nullptr;
		}
	private:
		Expr *expr1,*expr2;
		char op;
};



class Lvalue : public Expr{
	public:
		virtual Types get_data_type() =0 ;
};

class LvalueId : public Lvalue{
	public:
		LvalueId(std::string *var,Expr *expr):var(var),expr(expr){}
		void printAST(std::ostream &out) const override {
			out <<"LvalueId(";
				out<<*var;
				if(expr!=nullptr){
					out<<",["<<*expr<<"]";
				}
				out<<")";
		}
		void sem() override{
			VarEntry *v =STVar.lookup(var,line);
			if(expr!=nullptr){
				expr->sem();
				if(expr->get_type() != Type_int){
					yyerror("Index must be of type integer",line);
				}
				if(v->type==Type_byte_array){
					type=Type_byte;
				}
				else if(v->type==Type_int_array){
					type=Type_int;
				}
				else{
					yyerror("Cannot index a non array type",line);
				}
			}
			else{
				type=v->type;
			}
		}
		Value *compile() override{
			Value *base=IRSTVar.lookup(var)->val;
			if(expr==nullptr)
				return base;

			llvm::Value *index = expr->compile();
			llvm::Type *elementType;
			if(type==Type_int_array){
				elementType = i16;
			}
			else{
				elementType = i8;
			}
		    llvm::Value *elementPtr = Builder.CreateGEP(elementType,base, {c16(0), index}, "elementPtr");

			return Builder.CreateLoad(elementType,elementPtr, "elementValue");
		}
		Types get_data_type() override{
			if(type==Type_int_array || type==Type_byte_array){
				if(expr==nullptr){
					yyerror("Cannot assign a value to an entire array",line);
				}
				else{
					if(expr->get_type()!=Type_int){
						yyerror("Invalid index type, must be integer",line);
					}

					// TODO to check array boundaries ?
					if(type==Type_int_array){
						return Type_int;
					}
					else{
						return Type_byte;
					}

				}
			}
			return type;
		}
	private:
		std::string *var;
		Expr *expr;
};

class LvalueStr : public Lvalue{
	public:
		LvalueStr(std::string *var):var(var){}
		void printAST(std::ostream &out) const override {
			out <<"LvalueStr("<<*var<<")";
		}
		void sem() override{
			type=Type_byte_array;
		}
		Types get_data_type() override{
			yyerror("Cannot assign expression to string literal",line);
			return Type_byte_array;
		}
		Value * compile()override{
			llvm::GlobalVariable *stringVar = TheModule->getGlobalVariable(*var);
		    llvm::Constant *stringConstant;
			llvm::ArrayType *arrayType;
			if (!stringVar) {
			    arrayType = llvm::ArrayType::get(i8, var->size() + 1);
        		stringConstant = llvm::ConstantDataArray::getString(TheContext, *var, true);
				stringVar = new llvm::GlobalVariable(*TheModule,arrayType,true,llvm::GlobalValue::PrivateLinkage,stringConstant,*var);
			}
		return Builder.CreateGEP(arrayType,stringVar, {c16(0), c16(0)}, "stringPtr");
		}
	private:
		std::string *var;
};


class Stmt : public AST {
	public:
		virtual bool check_rtype(Rtypes type){
			return false;
		}
	protected:
	
};

class StmtList : public AST{
	public:
		StmtList(){}
		void append(Stmt * st) {
			stmt_list.push_back(st);
		}		
		void printAST(std::ostream &out) const override {
			bool first = 1;
			out << "StmtList(";
			for(const auto &it : stmt_list) {
				if(!first) {
					out << ",";
				} else 
					first = 0;
				out << *it;
			}
			out << ")";
		}
		void sem() override{

			for(const auto &i : stmt_list){
				i->sem();
			}
		}
		bool check_rtype(Rtypes type){
			bool ret=false;
			for(const auto &i : stmt_list){
				if(i->check_rtype(type)){
					ret=true;
				}
			}
			return ret;
		}
		Value * compile() override{
			for(auto &i : stmt_list ){
				i->compile();
			}
			return nullptr;
		}

	private:
		std::list<Stmt *> stmt_list;
};

class CompoundStmt : public Stmt{
    public:
        CompoundStmt(StmtList *stmt_list): stmt_list(stmt_list){}
        void printAST(std::ostream &out) const override{
            out<<"CompoundStmt{"<<*stmt_list<<"}";
        }
		void sem() override{
			stmt_list->sem();
		}

		bool check_rtype(Rtypes type) override{
			return stmt_list->check_rtype(type);
		}
		Value * compile() override{
			return stmt_list->compile();
		}
    private:
		StmtList *stmt_list;
};

 


class ExprList : public AST{
	public:
		ExprList(){}
		void append(Expr * ex) {
			expr_list.push_back(ex);
		}		
		void printAST(std::ostream &out) const override {
			bool first = true;
			out << "ExprList(";
			for(const auto &it : expr_list) {
				if(!first) {
					out << ",";
				} else 
					first = false;
				out << *it;
			}
		}
		void sem() override{
			for(const auto &i : expr_list){
				i->sem();
			}
		}
		std::vector<Types> get_list(){
			std::vector<Types> my_vector;
			for(const auto &i : expr_list){
				my_vector.push_back(i->get_type());
			}
			return my_vector;
		}
		std::vector<Value *> compile_list() {
			std::vector<Value *> my_vector;
			for(const auto &i : expr_list){
				my_vector.push_back(i->compile());
			}
			return my_vector;
		}

	private:
		std::list<Expr *> expr_list;
};





class FuncCall : public Expr{
	public:
		FuncCall(std::string *var,ExprList *expr_list):var(var),expr_list(expr_list){}
        void printAST(std::ostream &out) const override{
            out<<"FuncCall("<<*var<<":";
			if(expr_list!=nullptr){
				out<<*expr_list;
			}
			out<<")";
        }
		void sem() override{			
			if(expr_list!=nullptr){
				expr_list->sem();
				fun = STFun.lookup(var,expr_list->get_list(),line);
			}
			else{
				fun = STFun.lookup(var,{},line);
			}
		}
		Types get_type() override{
			if(fun->rtype==rtype_int){
				return Type_int;
			}
			else if (fun->rtype==rtype_byte){
				return Type_byte;
			}
			yyerror("Cannot have procedures as part of expressions\n",line);
			return Type_byte;
		}
		Value *compile()override{
			llvm::Function *CalleeF ;
			if(expr_list!=nullptr){
				CalleeF = IRSTFun.lookup(var,expr_list->get_list())->function;
			}
			else{
				CalleeF = IRSTFun.lookup(var,{})->function;
			}
			if(fun->rtype==Proc)
				return Builder.CreateCall(CalleeF, expr_list->compile_list());
			return Builder.CreateCall(CalleeF, expr_list->compile_list(), "calltmp");

			
		}
	private:
		std::string *var;
		FunEntry *fun;
		ExprList *expr_list;

};



class VarDef : public AST{
    public:
        VarDef(std::string *var,bool array,Datatype tp,int sz =0):vr(var),is_array(array),data_type(tp),size(sz){}
        void printAST(std::ostream &out) const override{
			out << "VarDef(" << *vr << ":" << data_type <<(is_array ?  "["+(std::to_string(size))+"]" : "") << ')';
        }
		void sem() override{
			if(is_array && size<1){
				yyerror("Invalid size for array\n",line);
			}
			Types type;
			if(!is_array){
				if(data_type==DATATYPE_int){
					type = Type_int;
				}
				else{
					type = Type_byte;
				}
			}
			else{
				if(data_type==DATATYPE_int){
					type = Type_int_array;
				}
				else{
					type = Type_byte_array;
				}
			}
			STVar.insert(vr,type,size,line);
		}
		Value *compile() override{
			    llvm::Type *llvmType;
				llvm::Value *llvmValue;

				if (is_array) {
					if (data_type == DATATYPE_int) {
						llvmType = llvm::ArrayType::get(i16, size);
					} else { 
						llvmType = llvm::ArrayType::get(i8, size);
					}
				} else {
					if (data_type == DATATYPE_int) {
						llvmType = i16;
					} else { 
						llvmType = i8;
					}
				}

				llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
				llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());

				llvmValue = TmpB.CreateAlloca(llvmType, nullptr, *vr);
				Types type;
				if(!is_array){
					if(data_type==DATATYPE_int){
						type = Type_int;
					}
					else{
						type = Type_byte;
					}
				}
				else{
					if(data_type==DATATYPE_int){
						type = Type_int_array;
					}
					else{
						type = Type_byte_array;
					}
				}
				IRSTVar.insert(vr,type,size,llvmValue);

				return llvmValue;  


		}
    private:
		std::string *vr;
		bool is_array;
		Datatype data_type;
		int size;
};

class FuncDef;

inline std::ostream &operator<<(std::ostream &out, const FuncDef &funcdef) ;



class LocalDef : public AST {
    public:
        LocalDef(VarDef * vr, FuncDef *func):var(vr),fun(func){}
        void printAST ( std::ostream &out) const override{
            if(fun==nullptr)
                out<<*var;
            else
                out<<*fun;
        }
 		void sem() override;
    private:
    VarDef * var;
    FuncDef * fun;

};


class LocalDefLr : public AST {
    public:
        LocalDefLr(){}
        void append (LocalDef * local_def){
            local_def_list.push_back(local_def);
        }
        void printAST(std::ostream &out) const override{
			bool first = true;
            for(const auto &it : local_def_list) {
				if(!first) {
					out << ",";
				} else 
					first = 0;
                out << *it;
            }
        }
		void sem() override{
			for (const auto &i : local_def_list){
				i->sem();
			}
		}
    private:
        std::vector<LocalDef *> local_def_list;
};



class FuncDef : public AST {
	public:
		FuncDef(FParList *fpl, RType *rtype,LocalDefLr *local_lr,CompoundStmt *stmt,std::string *id) : par_list(fpl), rtype(rtype),local_lr(local_lr),stmt(stmt),id(id) {}
		void printAST(std::ostream &out) const override {
			out << "Funcdef("; // << *id;
			out << *id <<",";
			if(par_list != nullptr) out << *par_list<<",";
			out << *rtype <<","<< *local_lr<<","<<*stmt<< ")";
		}
		void sem() override{
			if(par_list!=nullptr){
				par_list->sem();
				STFun.insert(id,par_list->get_type_list(),rtype->get_rtype(),line);
			}
			else{
				std::vector<Types> empty_vector={};
				STFun.insert(id,empty_vector,rtype->get_rtype(),line);
			}
			STFun.enterScope();
			STVar.enterScope();
			if(par_list!=nullptr){
				par_list->insert_params();
			}
			
			local_lr->sem();
			stmt->sem();
			if((!(stmt->check_rtype(rtype->get_rtype()))) && rtype->get_rtype()!=Proc){
				yyerror("Non proc function must contain a return statement in every path\n",line);
			}
			STFun.exitScope();
			STVar.exitScope();
		}
		llvm::Function * compile_main() override{
			return compile();
		}
		llvm::Function * compile() override{
			std::vector<llvm::Type*> llvmArgTypes;
			std::vector<Types> argTypes;
			if(par_list!=nullptr){
				argTypes=par_list->get_type_list();
			}
			for (Types type : argTypes) {
				switch (type) {
					case Type_int:     llvmArgTypes.push_back(llvm::Type::getInt32Ty(TheContext)); break;
					case Type_byte:    llvmArgTypes.push_back(llvm::Type::getInt8Ty(TheContext)); break;
					case Type_bool:    llvmArgTypes.push_back(llvm::Type::getInt1Ty(TheContext)); break;
					case Type_int_array: llvmArgTypes.push_back(llvm::ArrayType::get(llvm::Type::getInt32Ty(TheContext), 0)); break;
					case Type_byte_array: llvmArgTypes.push_back(llvm::ArrayType::get(llvm::Type::getInt8Ty(TheContext), 0)); break;
				}
			}

			llvm::Type* llvmReturnType;
			Rtypes returnType=rtype->get_rtype();
			switch (returnType) {
				case rtype_int:     llvmReturnType = llvm::Type::getInt32Ty(TheContext); break;
				case rtype_byte:    llvmReturnType = llvm::Type::getInt8Ty(TheContext); break;
				case Proc:          llvmReturnType = llvm::Type::getVoidTy(TheContext); break;
				default:            llvmReturnType = llvm::Type::getVoidTy(TheContext); // Default to void if not recognized
			}

			llvm::FunctionType* funcType = llvm::FunctionType::get(llvmReturnType, llvmArgTypes, false);
			if(!my_main){
				*id="main";
				funcType = llvm::FunctionType::get(i32, {}, false);
			}
			my_main=true;
			llvm::Function* function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, *id, TheModule.get());



			IRSTFun.insert(id, argTypes, returnType, function);

			IRSTFun.enterScope();
			IRSTVar.enterScope();
			std::vector<std::string *> params_name;
			if(par_list!=nullptr){
				params_name=par_list->get_name_list();
			}
			int index =0;
			for(auto &i : function->args()){
				IRSTVar.insert(params_name[index],argTypes[index],-1,&i);	
				index++;
			}
			local_lr->compile();
			llvm::BasicBlock *entry = llvm::BasicBlock::Create(TheContext, "function_entry", function);
			Builder.SetInsertPoint(entry);
			stmt->compile();
			IRSTFun.exitScope();
			IRSTVar.exitScope();

			return function;
		}
	private:
		FParList *par_list;
		RType *rtype;
		LocalDefLr *local_lr;
		CompoundStmt *stmt;
		std::string *id;

};




inline std::ostream &operator<<(std::ostream &out, const FuncDef &funcdef) {
	funcdef.printAST(out);
  	return out;
}







class Assignment: public Stmt {
    public:
        Assignment(Lvalue *lval,Expr *expr):lval(lval),expr(expr){}
        void printAST(std::ostream &out) const override{
            out<<"Assignment("<<*lval<<","<<*expr<<")";
        }
		void sem() override{
			lval->sem();
			expr->sem();
			if(expr->get_type()!=lval->get_data_type()){
				std::cout<<"Lhs is:"<<lval->get_data_type()<<"\nRhs is:"<<expr->get_type()<<std::endl;
				yyerror(" Both sides of an Assignment must be of the same type\n ",line);
			}
		}
		Value * compile() override{
			Value * lhs = lval->compile();
			Value * rhs = expr->compile();
			Builder.CreateStore(rhs, lhs);
			return nullptr;
		}
    private:
        Lvalue *lval;
        Expr *expr; 
};

class StmtFCall: public Stmt{
   public:
        StmtFCall(FuncCall  *fcall):fcall(fcall){}
        void printAST(std::ostream &out) const override{
            out<< *fcall;
        } 
		void sem() override{
			fcall->sem();
		}
		Value * compile() override{
			return fcall->compile();
		}
     private:
         FuncCall *fcall;
 };

class StmtIfCond : public Stmt{
	public:
		StmtIfCond(Cond *cond,Stmt *stmt1,Stmt *stmt2):cond(cond),stmt1(stmt1),stmt2(stmt2){}
        void printAST(std::ostream &out) const override{
			out<<"StmtCond("<<*cond<<","<<*stmt1;
			if(stmt2!=nullptr)
				out<<","<<*stmt2;
			out<<")";
        } 
		void sem() override{
			cond->sem();
			stmt1->sem();
			if(stmt2!=nullptr){
				stmt2->sem();
			}
		}
		bool check_rtype(Rtypes type) override{
			bool ret_if = stmt1->check_rtype(type);
			bool ret_else=false;
			if(stmt2!=nullptr){
				ret_else = stmt2->check_rtype(type);
			}
			return ret_if && ret_else;
		}
		Value * compile() override{
			    Value *v = cond->compile();
				Value *condit = Builder.CreateICmpNE(v, c1(0), "if_cond");
				llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
				llvm::BasicBlock *ThenBB =
				llvm::BasicBlock::Create(TheContext, "then", TheFunction);
				llvm::BasicBlock *ElseBB =
				llvm::BasicBlock::Create(TheContext, "else", TheFunction);
				llvm::BasicBlock *AfterBB =
				llvm::BasicBlock::Create(TheContext, "endif", TheFunction);
				Builder.CreateCondBr(condit, ThenBB, ElseBB);
				Builder.SetInsertPoint(ThenBB);
				stmt1->compile();
				Builder.CreateBr(AfterBB);
				Builder.SetInsertPoint(ElseBB);
				if (stmt2 != nullptr) stmt2->compile();
				Builder.CreateBr(AfterBB);
				Builder.SetInsertPoint(AfterBB);
				return nullptr;
		}
	private:
		Cond *cond;
		Stmt *stmt1;
		Stmt *stmt2;

};

class WhileStmt : public Stmt{
	public:
		WhileStmt(Cond *cond,Stmt *stmt):cond(cond),stmt(stmt){}
        void printAST(std::ostream &out) const override{
			out<<"StmtCond("<<*cond<<","<<*stmt<<")";
        } 
		void sem() override{
			cond->sem();
			stmt->sem();
		}
		bool check_rtype(Rtypes type) override{
			stmt->check_rtype(type);
			return false;
		}
		Value * compile()override{
	    	Value *n = cond->compile();
			llvm::BasicBlock *PrevBB = Builder.GetInsertBlock();
			llvm::Function *TheFunction = PrevBB->getParent();
			llvm::BasicBlock *LoopBB =
			llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
			llvm::BasicBlock *BodyBB =
			llvm::BasicBlock::Create(TheContext, "body", TheFunction);
			llvm::BasicBlock *AfterBB =
			llvm::BasicBlock::Create(TheContext, "endwhile", TheFunction);
			Builder.CreateBr(LoopBB);
			Builder.SetInsertPoint(LoopBB);
			llvm::PHINode *while_cond = Builder.CreatePHI(i32, 2, "iter");
			while_cond->addIncoming(n, PrevBB);
			Value *loop_cond =
			Builder.CreateICmpEQ(while_cond, c1(1), "loop_cond");
			Builder.CreateCondBr(loop_cond, BodyBB, AfterBB);
			Builder.SetInsertPoint(BodyBB);
			Value *remaining = cond->compile();
			stmt->compile();
			while_cond->addIncoming(remaining, Builder.GetInsertBlock());
			Builder.CreateBr(LoopBB);
			Builder.SetInsertPoint(AfterBB);
			return nullptr;
		}
	private:
		Cond *cond;
		Stmt *stmt;	
};

class EmptyStmt : public Stmt{
	public:
		EmptyStmt(){}
        void printAST(std::ostream &out) const override{
			out<<";";
        } 
		Value * compile() override{
			return nullptr;
		}
	private:
};


class StmtRet : public Stmt{
	public:
		StmtRet(Expr *exp):exp(exp){}
		void printAST(std::ostream &out) const override{
			out<<"Return(";
			if(exp!=nullptr)
				out<<*exp;
			out<<")";
		}
		void sem () override{
			if(exp!=nullptr)
				exp->sem();
		}
		bool check_rtype(Rtypes type) override{
			if( exp==nullptr){
				if(type!=Proc){
					yyerror("Error type mismatch in function definition\n",line);
				}
			}
			else{
				if(!((exp->get_type()==Type_int && type==rtype_int)||(exp->get_type()==Type_byte && type==rtype_byte))){
					yyerror("Error type mismatch in function definition\n",line);
				}
			}
			return true;
		}
		Value * compile() override{
			if(exp!=nullptr){
				Builder.CreateRet(exp->compile());
				return nullptr;
			}
			Builder.CreateRetVoid();
			return nullptr;
		}
	private:
		Expr *exp;
};







#endif
