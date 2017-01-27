SOURCE:=data_quality_report.md
TARGET:=data_quality_report.pdf
all:
	pandoc -s -N --toc  --listings -H listings-setup.tex --template=report.latex -V --number-section  -V papersize:a4paper -V geometry:margin=1.3in ${SOURCE} -o ${TARGET} 
