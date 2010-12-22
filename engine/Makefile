GAMBIT_HOME=/usr/local/gambit/MacOSX
GSC=${GAMBIT_HOME}/bin/gsc
GCC=gcc

GCC_FLAGS=-I${GAMBIT_HOME}/include -L${GAMBIT_HOME}/lib 

SCM_SRC_FILES=src/lists.scm
C_SRC_FILES=  src/lists.c
C_OBJ_FILES=  src/lists.o

all:
	${GSC} -:~~bin=${GAMBIT_HOME}/bin,~~lib=${GAMBIT_HOME}/lib,~~include=${GAMBIT_HOME}/include -f -link -o src/libdelectus.c ${SCM_SRC_FILES} > /dev/null
	${GSC} -:~~bin=${GAMBIT_HOME}/bin,~~lib=${GAMBIT_HOME}/lib,~~include=${GAMBIT_HOME}/include -obj ${C_SRC_FILES} src/libdelectus.c
	${GCC} ${GCC_FLAGS} -bundle ${C_OBJ_FILES} src/libdelectus.o -o src/libdelectus.dylib -lgambc

clean:
	rm -f src/*.c
	rm -f src/*.o
