all:
	happy -gca ParGramm.y
	alex -g LexGramm.x
	ghc --make TestGramm.hs -o TestGramm

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocGramm.* LexGramm.* ParGramm.* LayoutGramm.* SkelGramm.* PrintGramm.* TestGramm.* AbsGramm.* TestGramm ErrM.* SharedString.* ComposOp.* Gramm.dtd XMLGramm.* Makefile*
	

