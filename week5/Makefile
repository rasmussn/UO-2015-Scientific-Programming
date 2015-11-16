FC = mpif90
CC = cc
FFLAGS = -g
CFLAGS = -g

OBJS = frontier_list.o dijkstra.o LLdijkstra.o


all: dijkstra_main

dijkstra_main: dijkstra_main.f90 $(OBJS)
	$(FC) -o dijkstra $(FFLAGS) dijkstra_main.f90 $(OBJS) -Wl,-no_pie

dijkstra.o: dijkstra.F90
	$(FC) -c $(FFLAGS) dijkstra.F90

frontier_list.o: frontier_list.F90
	$(FC) -c $(FFLAGS) frontier_list.F90

LLdijkstra.o: LLdijkstra.c
	$(CC) -c $(CFLAGS) LLdijkstra.c

check: dijkstra_main
	@dijkstra

clean:
	rm -f *.o *.mod dijkstra
