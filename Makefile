.PHONY: clean

CC=g++
CFLAGS=-Wall

alan: lexer.o parser.o
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


clean:
	rm lex.yy.c lexer.c lexer.cpp parser.tab.h parser.tab.c parser.output *.o 
