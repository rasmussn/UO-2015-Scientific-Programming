FC = mpif90
CC = cc
FFLAGS = -g
CFLAGS = -g

OBJS = frontier_list.o dijkstra.o LLdijkstra.o


all: dijkstra_main test_mpi

dijkstra_main: dijkstra_main.o
	$(FC) -o dijkstra $(FFLAGS) dijkstra_main.F90 $(OBJS) -Wl,-no_pie

dijkstra_main.o: dijkstra_main.F90 $(OBJS)
	$(FC) -c $(FFLAGS) dijkstra_main.F90

dijkstra.o: dijkstra.F90
	$(FC) -c $(FFLAGS) dijkstra.F90

frontier_list.o: frontier_list.F90
	$(FC) -c $(FFLAGS) frontier_list.F90

LLdijkstra.o: LLdijkstra.c
	$(CC) -c $(CFLAGS) LLdijkstra.c

test_mpi: test_mpi.F90
	$(FC) -o test_mpi $(FFLAGS) test_mpi.F90

check: dijkstra_main
	@dijkstra

checkmpi: test_mpi
	@mpirun -np 2 test_mpi

clean:
	rm -f *.o *.mod *.i90 *.dwarf dijkstra test_mpi
