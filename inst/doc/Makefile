default:
	make standard
	make clean

standard:
	make s3x.pdf

s3x.pdf: maia4.sty s3x.tex
	pdflatex s3x.tex

clean:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f tmp*
	rm -f Rplots*

