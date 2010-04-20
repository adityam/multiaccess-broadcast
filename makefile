all: value-function.pdf

value-function.mac: value-function.txt
	cat value-function.txt | runhaskell extract-code.hs > value-function.mac

result.txt: value-function.mac
	maxima --batch-string="batchload(\"value-function.mac\");" > result.txt

value-function.tex: value-function.txt
	pandoc -w context value-function.txt > value-function.tex

value-function.pdf: value-function.tex wrapper.tex result.txt
	texexec wrapper.tex --result=value-function
