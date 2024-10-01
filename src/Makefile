CC = gcc
CFLAGS = `pkg-config --cflags guile-3.0`
LIBS=`pkg-config --libs guile-3.0`

all :  task12 task19
task12: task12.o
	${CC} 12_withc.o ${LIBS} -o 12_withc
task12.o: 12_withc.c
	${CC} -c ${CFLAGS} 12_withc.c 
task19: task19.o
	${CC} 19_withc.o ${LIBS} -o 19_withc
task19.o: 19_withc.c
	${CC} -c ${CFLAGS} 19_withc.c
clean: 
	-rm 12_withc 19_withc *.o 

