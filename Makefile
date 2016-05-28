all:
	bnfc SFL.cf
	happy -gca ParSFL.y
	alex -g LexSFL.x
#	ghc --make TestSFL.hs -o TestSFL
	ghc --make Interpreter/Main.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm Interpreter/*.hi Interpreter/*.o
	-rm StdLib/*.hi StdLib/*.o
	-rm TypeChecker/*.hi TypeChecker/*.o

distclean: clean
	-rm -f DocSFL.* LexSFL.* ParSFL.* LayoutSFL.* SkelSFL.* PrintSFL.* TestSFL.* AbsSFL.* TestSFL ErrM.* SharedString.* ComposOp.* SFL.dtd XMLSFL.* interpreter
	

