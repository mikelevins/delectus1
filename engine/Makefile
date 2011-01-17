GAMBIT_HOME=/usr/local/gambit/MacOSX
GSC=${GAMBIT_HOME}/bin/gsc
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib
GCC = gcc

FLAGS_OBJ = -I${GSC_INC} -L${GSC_LIB} -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp 

FLAGS_LIB =  -I${GSC_INC} -L${GSC_LIB} -flat_namespace -undefined suppress

FLAGS_EXE =  -I${GSC_INC} -L${GSC_LIB} -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp  

SCM_SOURCES = lib/uuid.scm\
	      src/lists.scm src/sequences.scm src/vectors.scm\
	      src/constants.scm src/values.scm\
	      src/tables.scm\
	      src/views.scm src/documents.scm src/document-registry.scm\
	      src/csv.scm\
              src/api.scm src/delectus.scm

C_SOURCES = lib/uuid.c\
	    src/lists.c src/sequences.c src/vectors.c\
	    src/constants.c src/values.c\
	    src/tables.c\
	    src/views.c src/documents.c src/document-registry.c\
	    src/csv.c\
            src/api.c src/delectus.c src/delectus_.c

C_OBJECTS = uuid.o\
	    lists.o sequences.o vectors.o\
	    constants.o values.o\
	    tables.o\
	    views.o documents.o document-registry.o\
	    csv.o\
            api.o delectus.o delectus_.o

exe: lib
	${GCC} ${FLAGS_EXE} -I"${GSC_INC}" -I./include test/delectus_test.c -o delectus_test -L${GSC_LIB} -L. -ldelectus -lgambc

lib: obj
	ar rc libdelectus.a ${C_OBJECTS} && ranlib libdelectus.a

obj: compile_scheme
	${GCC} ${FLAGS_OBJ} -I"${GSC_INC}" -c ${C_SOURCES}  -D___LIBRARY

compile_scheme:
	${GSC} -:~~bin=${GSC_BIN},~~lib=${GSC_LIB},~~include=${GSC_INC} -f -link ${SCM_SOURCES}

clean:
	rm -f ${C_SOURCES}
	rm -f libdelectus.a delectus_test
	rm -f *.o
	rm -f src/*~
	rm -f *~
