FC = gfortran

OBJS = array_syntax.o
EXES = array_syntax

all: $(EXES)

array_syntax.o: array_syntax.f90
	$(FC) -c -g array_syntax.f90

array_syntax: array_syntax.o
	$(FC) -o array_syntax array_syntax.o

clean:
	rm -f *.o array_syntax
