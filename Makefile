.PHONY: clean

LLVMCONFIG=llvm-config
CC=g++
CFLAGS=-Wall -g `$(LLVMCONFIG) --cxxflags`

LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

alan: parser.o lexer.o  ast.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

lexer.o: lexer.cpp lexer.hpp parser.tab.h 
	$(CC) $(CFLAGS) -c -o $@ lexer.cpp 

lexer.cpp: lexer.l
	flex -s -o $@ $<

parser.tab.h parser.tab.c : parser.y
	bison -dv -Wall -Wconflicts-sr $< 

parser.o : parser.tab.c
	$(CC) $(CFLAGS) -c -o $@ $<

lexer : lexer.l parser.tab.h
	flex -s -o $@.cpp $<
	$(CC) -o $@ lexer.cpp -DTOKEN_DBG
	
ast.o: ast.cpp
	$(CC) $(CFLAGS) -c -o $@ $< 


clean:
	rm  lexer.cpp parser.tab.h parser.tab.c parser.output *.o 
