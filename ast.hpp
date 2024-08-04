//TODO CHECK ALL FUNCTIONS HAVE RETURN STATEMENTS



#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <string.h>
#include <vector>
#include <map>
#include <list>
#include <pair>

#include "symbol.hpp"

extern std::map<std::string, int> global_vars;

class AST {
	public:
		virtual void printAST(std::ostream &out) const = 0;
		virtual ~AST() = default;
		virtual void sem(){};
};

//declarations 

//class FuncDef ;


inline std::ostream &operator<<(std::ostream &out, Datatype t) {
	switch(t) {
		case DATATYPE_int:  out << "int";  break;
		case DATATYPE_byte: out << "byte"; break;
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
				if(dt==DATATYPE_byte){
					type = Type_byte_array;
				}
				else{
					type = Type_int_array;	
				}
			}
			else{
				if(dt== DATATYPE_byte){
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
				if(dt==DATATYPE_int){
					rtype= rtype_int;
				}
				else{
					rtype=rtype_byte;
				}
			}
			return rtype;
		}
		private:
			Datatype datatype;
			bool isProc;
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
			SymbolTableVar.insert(*var,type->get_type,-1);
		}
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
			if(expr1->get_type()!=Type_int || expr1->get_type()!=Type_byte ){
				yyerror("Binary operators can only be applied to integers and bytes\n");
			}
			if(expr1->get_type()!=expr2->get_type()){
				yyerror("Operands must be of the same type\n");
			}

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
	private:
		char var;
};

class ExprLitInt : public Expr{
	public:
		ExprLitInt(char lit):lit(lit){}
		void printAST(std::ostream &out) const override {
			out <<"ExprLitInt("<<lit<<")";
		}
		void sem() override{
			type = Type_int; 
		}
	private:
		int lit;
};


// class ExprFunCall : public Expr{
// 	public:
// 		ExprFunCall(FuncCall *func_call):func_call(func_call){}
// 		void printAST(std::ostream &out) const override {
// 			out <<"ExprFunCall("<<*func_call<<")";
// 		}
// 	private:
// 		FuncCall *func_call;
// };

// class ExprLval : public Expr{
// 	public:
// 		ExprLval(Lvalue *lval):lval(lval){}
// 		void printAST(std::ostream &out) const override {
// 			out <<"ExprLval("<<*lval<<")";
// 		}	
// 	private:
// 		Lvalue *lval;
// };

class ExprUnitaryOp : public Expr{
	public:
		ExprUnitaryOp(Expr *expr,char op):expr(expr),op(op){}
		void printAST(std::ostream &out) const override {
			out <<"ExprUnitaryOp("<<*expr<<")";
		}	
		void sem() override{
			expr->sem();
			if(expr->get_type()!=Type_int){
				yyerror("+ or - can only be applied to integers\n");
			}
			type=Type_int; 
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
			if(expr1->get_type()!=Type_int || expr1->get_type()!=Type_byte ){
				yyerror("Binary operators can only be applied to integers and bytes\n");
			}
			if(expr1->get_type()!=expr2->get_type()){
				yyerror("Operands must be of the same type\n");
			}
			type = expr1->get_type();

			// TODO CHECK DIVISION BY 0 
		
		}
	private:
		Expr *expr1,*expr2;
		char op;
};



class Lvalue : public Expr{};

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
			VarEntry v =SymbolTableVar.lookup(*var);
			type=v.type;
		}
		Types get_data_type(){
			if(v.type==Type_int_array || v.type==Type_byte_array){
				if(expr==nullptr){
					yyerror("Cannot assign a value to an entire array");
				}
				else{
					if(expr->get_type()!=Type_int){
						yyerror("Invalid index type, must be integer");
					}

					// TODO to check array boundaries ?
					if(v.type==Type_int_array){
						return Type_int;
					}
					else{
						return Type_byte;
					}

				}
			}
			return v.type;
		}
	private:
		std::string *var;
		Expr *expr;
		VarEntry v;
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
		Types get_data_type(){
			yyerror("Cannot assign expression to string literal");
			return Type_byte_array;
		}
	private:
		std::string *var;
};


class Stmt : public AST {
	public:
		virtual void check_rtype(Rtypes type){}
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
		void check_rtype(Rtypes type) override{
			for(const auto &i : stmt_list){
				i->check_rtype(type);
			}
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

		void check_rtype(Rtypes type) override{
			stmt_list->check_rtype(type);
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
			expr_list->sem();
			fun = SymbolTableFun.lookup(*var,expr_list->get_list());
			if(fun.type == )
			type = fun.type;

		}
		Types get_type() override{
			if(fun.type==rtype_int){
				return Type_int;
			}
			else if (fun.type==rtype_byte){
				return Type_byte;
			}
			yyerror("Cannot have procedures as part of expressions\n");
			return Type_byte;
		}
	private:
		std::string *var;
		FunEntry fun;
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
				yyerror("Invalid size for array\n");
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
			SymbolTableVar.insert(*vr,type,size);
		}
    private:
		bool is_array;
		int size;
		std::string *vr;
		Datatype data_type;
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
		void sem() override{
			if(fun==nullptr){
				var->sem();
			}
			else{
				fun->sem();
			}
		}
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
		FuncDef(FParList *fpl, RType *rtype,LocalDefLr *local_lr,CompoundStmt *stmt,std::string *id) : par_list(fpl), rtype(rtype),stmt(stmt),local_lr(local_lr),id(id) {}
		void printAST(std::ostream &out) const override {
			out << "Funcdef("; // << *id;
			out << *id <<",";
			if(par_list != nullptr) out << *par_list<<",";
			out << *rtype <<","<< *local_lr<<","<<*stmt<< ")";
		}
		void sem() override{
			par_list->sem();
			SymbolTableFun.insert(*id,par_list->get_list(),rtype->get_rtype);
			SymbolTableFun.enterScope();
			SymbolTableVar.enterScope();
			par_list->insert_params();
			local_def->sem();
			stmt->sem();
			stmt->check_rtype(rtype->get_rtype);
			SymbolTableFun.exitScope();
			SymbolTableVar.exitScope();
		}

	private:
		std::string *id;
		FParList *par_list;
		RType *rtype;
        LocalDefLr *local_lr;
        CompoundStmt *stmt;

};


inline std::ostream &operator<<(std::ostream &out, const FuncDef &funcdef) {
	funcdef.printAST(out);f
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
			expr->check_type(lval->get_data_type());
		}
    private:
        Lvalue *lval;
        Expr *expr; 
};

// class StmtCompound: public Stmt{
//     public:
//         StmtCompound(CompoundStmt  *comstmt):comstmt(comstmt){}
//         void printAST(std::ostream &out) const override{
//             out<< *comstmt;
//         }
//     private:
//         CompoundStmt  *comstmt;
// };

class StmtFCall: public Stmt{
   public:
        StmtFCall(FuncCall  *fcall):fcall(fcall){}
        void printAST(std::ostream &out) const override{
            out<< *fcall;
        } 
		void sem() override{
			fcall->sem();
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
		void check_rtype(Rtypes type) override{
			stmt1->check_rtype(type);
			if(stmt2!=nullptr){
				stmt2->check_rtype(type);
			}
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
		void check_rtype(Rtypes type) override{
			stmt->check_rtype(type);
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
			exp->sem();
		}
		void check_rtype(Rtypes type) override{
			if( exp==nullptr){
				if(type!=Proc){
					yyerror("Error type mismatch in function definition\n");
				}
			}
			else{
				if(!((exp->get_type()==Type_int && type==rtype_int)||(exp->get_type()==Type_byte && type==rtype_byte))){
					yyerror("Error type mismatch in function definition\n");
				}
			}
		}
	private:
		Expr *exp;
};







#endif
