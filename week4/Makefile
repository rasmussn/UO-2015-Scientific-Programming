FC = mpif90
FFLAGS = -O3

OBJS = dijkstra.o


all: dijkstra_main

dijkstra_main: dijkstra_main.f90 $(OBJS)
	$(FC) -o dijkstra $(FFLAGS) dijkstra_main.f90 $(OBJS)

dijkstra.o: dijkstra.F90
	$(FC) -c $(FFLAGS) dijkstra.F90

check: dijkstra_main
	@dijkstra

clean:
	rm -f *.o *.mod dijkstra
