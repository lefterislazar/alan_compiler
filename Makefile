.PHONY: clean

CC=g++
CFLAGS=-Wall

alan: parser.o lexer.o  ast.o
	$(CC) $(CFLAGS) -o $@ $^

lexer.o: lexer.cpp lexer.hpp parser.tab.h 
	$(CC) -c -o $@ lexer.cpp 

lexer.cpp: lexer.l
	flex -s -o $@ $<

parser.tab.h parser.tab.c : parser.y
	bison -dv -Wall -Wconflicts-sr $< 

parser.o : parser.tab.c
	$(CC) -c -o $@ $<

lexer : lexer.l parser.tab.h
	flex -s -o $@.cpp $<
	$(CC) -o $@ lexer.cpp -DTOKEN_DBG
	
ast.o: ast.cpp
	$(CC) $(CFLAGS) -c -o $@ $< 


clean:
	rm  lexer.cpp parser.tab.h parser.tab.c parser.output *.o 
