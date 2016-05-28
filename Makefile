all: bnfc 
	stack build

bnfc:
	bnfc SFL.cf

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocSFL.* LexSFL.* ParSFL.* LayoutSFL.* SkelSFL.* PrintSFL.* TestSFL.* AbsSFL.* TestSFL ErrM.* SharedString.* ComposOp.* SFL.dtd XMLSFL.*
	

