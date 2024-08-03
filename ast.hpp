#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <string.h>
#include <vector>
#include <map>
#include <list>

#include "symbol.hpp"

extern std::map<std::string, int> global_vars;

class AST {
	public:
		virtual void printAST(std::ostream &out) const = 0;
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
	private:
		Datatype datatype;
		bool isArray;
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


	private:
		std::list<FParDef *> pardef_list;
};



//class Expr

class Expr : public AST{
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
	private:
		bool cond;
};

class CompareCond : public Cond{
	public:
		CompareCond(Expr *expr1,Expr *expr2,char op):expr1(expr1),expr2(expr2),op(op){}
		void printAST(std::ostream &out) const override {
			out <<"CompareCond("<<*expr1<<","<<op<<","<<*expr2<<")";
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
	private:
		char var;
};

class ExprLitInt : public Expr{
	public:
		ExprLitInt(char lit):lit(lit){}
		void printAST(std::ostream &out) const override {
			out <<"ExprLitInt("<<lit<<")";
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
	private:
		Expr *expr1,*expr2;
		char op;
};


// TODO: ID localdeflr compoundstmt

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
	private:
		std::string *var;
};


class Stmt : public AST {
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


	private:
		std::list<Stmt *> stmt_list;
};

class CompoundStmt : public Stmt{
    public:
        CompoundStmt(StmtList *stmt_list): stmt_list(stmt_list){}
        void printAST(std::ostream &out) const override{
            out<<"CompoundStmt{"<<*stmt_list<<"}";
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
	private:
		std::string *var;
		ExprList *expr_list;

};


class VarDef : public AST{
    public:
        VarDef(std::string *var,bool array,Datatype tp,int sz =0):vr(var),is_array(array),data_type(tp),size(sz){}
        void printAST(std::ostream &out) const override{
			out << "VarDef(" << *vr << ":" << data_type <<(is_array ?  "["+(std::to_string(size))+"]" : "") << ')';
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

	private:
		std::string *id;
		FParList *par_list;
		RType *rtype;
        LocalDefLr *local_lr;
        CompoundStmt *stmt;

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
	private:
		Expr *exp;
};







#endif
