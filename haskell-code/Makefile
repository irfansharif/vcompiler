.PHONY: all

all:
	ghc -o wave/regex Main
	rm *.hi *.o

clean: 
	rm *.hi *.o Main

test-regex:
	cd wave && ./regex $(ls *wave | tr "\n" " ")
