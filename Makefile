make:
	pdflatex tex/Auxx.tex

make clean:
	rm tex/Auxx.aux && rm tex/Auxx.log && rm tex/Auxx.pdf	&& rm tex/Auxx.toc
