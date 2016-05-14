all: bnfc 
	ghc --make Interpreter/Main.hs -o interpreter

bnfc:
	bnfc SFL.cf
	happy -gca ParSFL.y
	alex -g LexSFL.x
	ghc --make TestSFL.hs -o TestSFL

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi Interpreter/*.o Interpreter/*.hi TypeChecker/*.o TypeChecker/*.hi StdLib/*.o StdLib/*.hi 

distclean: clean
	-rm -f DocSFL.* LexSFL.* ParSFL.* LayoutSFL.* SkelSFL.* PrintSFL.* TestSFL.* AbsSFL.* TestSFL ErrM.* SharedString.* ComposOp.* SFL.dtd XMLSFL.*
	

