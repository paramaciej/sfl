all: interpreter

stack:
	stack setup

interpreter: bnfc
	stack install --local-bin-path=$(shell pwd)

bnfc: stack
	stack install BNFC
	stack exec bnfc SFL.cf

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi interpreter

distclean: clean
	-rm -rf DocSFL.* LexSFL.* ParSFL.* LayoutSFL=.* SkelSFL.* PrintSFL.* TestSFL.* AbsSFL.* TestSFL ErrM.* SharedString.* ComposOp.* SFL.dtd XMLSFL.* .stack-work
	

