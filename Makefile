all:
	ghc --make Main.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

