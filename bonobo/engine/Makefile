GAMBIT_HOME=/usr/local/gambit/MacOSX
GSC=${GAMBIT_HOME}/bin/gsc
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib
GCC = gcc

SCHEME_SOURCES = scm/Sort.scm scm/api.scm scm/constants.scm scm/csv.scm \
                 scm/data.scm scm/documents.scm scm/functions.scm \
                 scm/io-formats.scm scm/io.scm scm/lists.scm scm/strings.scm \
                 scm/uuid.scm scm/vectors.scm scm/views.scm scm/Delectus.scm

C_SOURCES = scm/Sort.c scm/api.c scm/constants.c scm/csv.c \
            scm/data.c scm/documents.c scm/functions.c \
            scm/io-formats.c scm/io.c scm/lists.c scm/strings.c \
            scm/uuid.c scm/vectors.c scm/views.c scm/Delectus.c scm/Delectus_.c

OBJECTS = Sort.o api.o constants.o csv.o \
          data.o documents.o functions.o \
          io-formats.o io.o lists.o strings.o \
          uuid.o vectors.o views.o Delectus.o Delectus_.o

PRODUCT = libDelectus.a

FLAGS_OBJ = -I${GSC_INC} -L${GSC_LIB} -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp 

lib: obj
	ar rc ${PRODUCT} ${OBJECTS} && ranlib ${PRODUCT}
	rm -f ${C_SOURCES}
	rm -f ${OBJECTS}

obj: compile_scheme
	${GCC} ${FLAGS_OBJ} -I"${GSC_INC}" -c ${C_SOURCES} ${GSC_LIB}/libgambc.a  -D___LIBRARY

compile_scheme:
	${GSC} -:~~bin=${GSC_BIN},~~lib=${GSC_LIB},~~include=${GSC_INC} -f -link ${SCHEME_SOURCES}

clean:
	rm -f ${C_SOURCES}
	rm -f ${OBJECTS}
	rm -f *~
