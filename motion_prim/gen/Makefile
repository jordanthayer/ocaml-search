CC=gcc
CFLAGS=-c -Wall
CFLAGS2=-lgsl -lgslcblas -lm

all: diff_drive hovercraft

hovercraft: hovercraft.o
	$(CC) $(CFLAGS2) hovercraft.o -o hovercraft.Unix

diff_drive: diff_drive.o
	$(CC) $(CFLAGS2) diff_drive.o -o diff_drive.Unix

hovercraft.o: hovercraft.c
	$(CC) $(CFLAGS) hovercraft.c

diff_drive.o: diff_drive.c
	$(CC) $(CFLAGS) diff_drive.c

clean:
	rm -rf *o hello
