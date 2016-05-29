all: interpreter

stack:
	wget -O stack.tar.gz https://www.stackage.org/stack/linux-x86_64
	tar xf stack.tar.gz
	rm -rf stack-local
	mv stack-1* stack-local
	SYSTEM_CERTIFICATE_PATH=/etc/openssl/certs ./stack-local/stack setup

interpreter: bnfc
	SYSTEM_CERTIFICATE_PATH=/etc/openssl/certs ./stack-local/stack install --local-bin-path=$(shell pwd)

bnfc: stack
	SYSTEM_CERTIFICATE_PATH=/etc/openssl/certs ./stack-local/stack install BNFC
	SYSTEM_CERTIFICATE_PATH=/etc/openssl/certs ./stack-local/stack exec bnfc SFL.cf

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocSFL.* LexSFL.* ParSFL.* LayoutSFL.* SkelSFL.* PrintSFL.* TestSFL.* AbsSFL.* TestSFL ErrM.* SharedString.* ComposOp.* SFL.dtd XMLSFL.*
	

