CC = cc
CFLAGS = -g -I/usr/local/include
LDFLAGS = -L/usr/local/lib -lnetcdf

OBJS = dijkstra.o

all: dijkstra_main

dijkstra_main.o: dijkstra_main.c
	$(CC) -c dijkstra_main.c $(CFLAGS)

dijkstra_main: dijkstra_main.o $(OBJS)
	$(CC) -o dijkstra $(LDFLAGS) dijkstra_main.o $(OBJS)

dijkstra.o: dijkstra.c
	$(CC) -c $(CFLAGS) dijkstra.c

check: dijkstra_main
	@dijkstra -i srModel_forTomographyPaper.nc

clean:
	rm -f *.o dijkstra
