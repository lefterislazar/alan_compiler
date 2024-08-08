#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

extern void yyerror(const char *msg,int line);


enum Datatype { DATATYPE_int, DATATYPE_byte };

enum Types { Type_int, Type_byte, Type_bool , Type_int_array , Type_byte_array };

enum Rtypes {  Proc , rtype_int , rtype_byte };


struct VarEntry{
    Types type;
    int size;
    VarEntry (){}
    VarEntry(Types type , int size):type(type),size(size){}
};

struct FunEntry{
    Rtypes rtype;
    FunEntry (){}
    FunEntry (Rtypes rtype):rtype(rtype){}
};

class VarScope{
    public:
        VarScope(){}
        void insert (std::string *var,Types type,int size,int line){
            if(locals.find(*var)!= locals.end()){
                std::string my_error = "Duplicate variable detection with id: "+*var;
                yyerror(my_error.c_str(),line);
            }
            locals[*var] = VarEntry(type,size);
        }
        VarEntry *lookup(std::string *var){
            if(locals.find(*var) == locals.end()) return nullptr;
            return &(locals[*var]);
        }
    private:
        std::map<std::string,VarEntry> locals;
};



class SymbolTableVar{
 public:
    void insert(std::string *v, Types t,int size,int line) {
        scopes.back().insert(v, t,size,line);
    }
    VarEntry *lookup(std::string *v,int line) {
        for (auto s = scopes.rbegin(); s != scopes.rend(); ++s) {
            VarEntry *e = s->lookup(v);
            if (e != nullptr) return e;
        }
        std::string my_error = "Variable "+*v+" not found";
        yyerror(my_error.c_str(),line);
        return nullptr;
    }
    void enterScope() {
        scopes.push_back(VarScope());
    }
    void exitScope() {
        scopes.pop_back();
    }
    private:
    std::vector<VarScope> scopes;
};


class FunScope{
    public:
        FunScope(){}
        void insert (std::string *id,std::vector<Types> parameters, Rtypes type,int line){
            std::pair<std::string,std::vector<Types>> var(*id,parameters);
            if(locals.find(var) != locals.end()){
               yyerror("Duplicate function declaration",line);
            }
            locals[var] = FunEntry(type);
        }
        FunEntry *lookup(std::string *id,std::vector<Types> parameters){
            std::pair<std::string,std::vector<Types>> var(*id,parameters);
            if(locals.find(var) == locals.end()){
                return nullptr;
            }
            return &(locals[var]);
        }
    private:
        std::map<std::pair<std::string,std::vector<Types>>,FunEntry> locals;
};


class SymbolTableFun{
    public:
        SymbolTableFun(){
            enterScope();
            std::string id="writeInteger";
            std::vector<Types> types = {Type_int}; 
            insert(&id,types,Proc);
            id="writeByte";
            types = {Type_byte}; 
            insert(&id,types,Proc);
            id="writeChar";
            types = {Type_byte}; 
            insert(&id,types,Proc);
            id="writeString";
            types = {Type_byte_array}; 
            insert(&id,types,Proc);


            id="readInteger";
            types = {}; 
            insert(&id,types,rtype_int);
            id="readByte";
            types = {}; 
            insert(&id,types,rtype_byte);
            id="readChar";
            types = {}; 
            insert(&id,types,rtype_byte);
            id="readString";
            types = {Type_int,Type_byte_array}; 
            insert(&id,types,Proc);

            id="extend";
            types = {Type_byte}; 
            insert(&id,types,rtype_int);
            id="shrink";
            types = {Type_int}; 
            insert(&id,types,rtype_byte);


            id="strlen";
            types = {Type_byte_array}; 
            insert(&id,types,rtype_int);
            id="strcmp";
            types = {Type_byte_array,Type_byte_array}; 
            insert(&id,types,rtype_int);
            id="strcpy";
            types = {Type_byte_array,Type_byte_array}; 
            insert(&id,types,Proc);
            id="strcat";
            types = {Type_byte_array,Type_byte_array}; 
            insert(&id,types,Proc);

        }
        ~SymbolTableFun(){
            exitScope();
        }
        void insert(std::string *id,std::vector<Types> parameters, Rtypes type,int line=0) {
            scopes.back().insert(id, parameters,type,line);
        }
        FunEntry *lookup(std::string *id,std::vector<Types> parameters,int line) {
            for (auto s = scopes.rbegin(); s != scopes.rend(); ++s) {
            FunEntry *e = s->lookup(id,parameters);
            if (e != nullptr) return e;
            }
            yyerror("Function not found",line);
            return nullptr;
        }
        void enterScope() {
            scopes.push_back(FunScope());
        }
        void exitScope() {
            scopes.pop_back();
        }
    private:
        std::vector<FunScope> scopes;
};

 extern SymbolTableFun STFun;
 extern SymbolTableVar STVar;


/****************************************************** 

Symbol Table for LLVM

******************************************************/




struct IRVarEntry{
    Types type;
    int size;
    llvm::Value * val;
    IRVarEntry (){}
    IRVarEntry(Types type , int size,llvm::Value * val):type(type),size(size),val(val){}
};

struct IRFunEntry{
    Rtypes rtype;
    llvm::Function *function;
    IRFunEntry (){}
    IRFunEntry (Rtypes rtype,llvm::Function *fun=nullptr):rtype(rtype),function(fun){}
};

class IRVarScope{
    public:
        IRVarScope(){}
        void insert (std::string *var,Types type,int size,llvm::Value *v){
            locals[*var] = IRVarEntry(type,size,v);
        }
        IRVarEntry *lookup(std::string *var){
            if(locals.find(*var) == locals.end()) return nullptr;
            return &(locals[*var]);
        }
    private:
        std::map<std::string,IRVarEntry> locals;
};



class IRSymbolTableVar{
 public:
    void insert(std::string *v, Types t,int size, llvm::Value *val) {
        scopes.back().insert(v, t,size,val);
    }
    IRVarEntry *lookup(std::string *v) {
        for (auto s = scopes.rbegin(); s != scopes.rend(); ++s) {
            IRVarEntry *e = s->lookup(v);
            if (e != nullptr) return e;
        }
        return nullptr;
    }
    void enterScope() {
        scopes.push_back(IRVarScope());
    }
    void exitScope() {
        scopes.pop_back();
    }
    private:
    std::vector<IRVarScope> scopes;
};


class IRFunScope{
    public:
        IRFunScope(){}
        void insert (std::string *id,std::vector<Types> parameters, Rtypes type,llvm::Function *entry){
            std::pair<std::string,std::vector<Types>> var(*id,parameters);
            locals[var] = IRFunEntry(type,entry);
        }
        IRFunEntry *lookup(std::string *id,std::vector<Types> parameters){
            std::pair<std::string,std::vector<Types>> var(*id,parameters);
            if(locals.find(var) == locals.end()){
                return nullptr;
            }
            return &(locals[var]);
        }
    private:
        std::map<std::pair<std::string,std::vector<Types>>,IRFunEntry> locals;
};


class IRSymbolTableFun{
    public:
        IRSymbolTableFun(){
            enterScope();
        }
        ~IRSymbolTableFun(){
            exitScope();
        }
        void insert(std::string *id,std::vector<Types> parameters, Rtypes type,llvm::Function *function) {
            scopes.back().insert(id, parameters,type,function);
        }
        IRFunEntry *lookup(std::string *id,std::vector<Types> parameters) {
            for (auto s = scopes.rbegin(); s != scopes.rend(); ++s) {
                IRFunEntry *e = s->lookup(id,parameters);
                if (e != nullptr) return e;
            }
            return nullptr;
        }
        void enterScope() {
            scopes.push_back(IRFunScope());
        }
        void exitScope() {
            scopes.pop_back();
        }
    private:
        std::vector<IRFunScope> scopes;
};

 extern IRSymbolTableFun IRSTFun;
 extern IRSymbolTableVar IRSTVar;

#endif
