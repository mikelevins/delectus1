GAMBIT_HOME=/Library/Gambit/current
GSC=${GAMBIT_HOME}/bin/gsc
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib
GCC = gcc

SCHEME_SOURCES = scm/base/constants.scm \
		 scm/lib/lists.scm scm/lib/vectors.scm scm/lib/strings.scm \
                 scm/lib/Sort.scm scm/lib/sort-keys.scm scm/lib/filter-keys.scm \
		 scm/lib/functions.scm \
	         scm/api/api.scm \
                 scm/data/entries.scm scm/data/rows.scm scm/data/columns.scm \
                 scm/data/tables.scm scm/data/views.scm scm/data/registry.scm \
                 scm/api/engine.scm \
                 scm/io/csv.scm scm/io/io-formats.scm scm/io/io.scm  \
                 scm/api/Delectus.scm

C_SOURCES = scm/base/constants.c \
            scm/lib/lists.c scm/lib/vectors.c scm/lib/strings.c \
            scm/lib/Sort.c scm/lib/sort-keys.c scm/lib/filter-keys.c \
            scm/lib/functions.c \
            scm/api/api.c \
            scm/data/entries.c scm/data/rows.c scm/data/columns.c \
            scm/data/tables.c scm/data/views.c scm/data/registry.c \
            scm/api/engine.c \
            scm/io/csv.c scm/io/io-formats.c scm/io/io.c  \
            scm/api/Delectus.c scm/api/Delectus_.c

OBJECTS = constants.o \
          lists.o vectors.o strings.o \
          Sort.o sort-keys.o filter-keys.o \
          functions.o \
          api.o \
          entries.o rows.o columns.o \
          tables.o views.o registry.o \
          engine.o \
          csv.o io-formats.o io.o  \
          Delectus.o Delectus_.o

PRODUCT = libDelectus.a
DYLIB = libDelectus.dylib

FLAGS_OBJ = -I${GSC_INC} -L${GSC_LIB} -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp 

lib: obj
	ar rc ${PRODUCT} ${OBJECTS} && ranlib ${PRODUCT}
	rm -f ${C_SOURCES}
	rm -f ${OBJECTS}

shared_lib: shared_obj
	${GCC} -shared ${OBJECTS} -lgambit -o ${DYLIB}

obj: compile_scheme
	${GCC} ${FLAGS_OBJ} -I"${GSC_INC}" -c ${C_SOURCES} ${GSC_LIB}/libgambit.a -D___LIBRARY

shared_obj: compile_scheme
	${GCC} ${FLAGS_OBJ} -I"${GSC_INC}" -c ${C_SOURCES} ${GSC_LIB}/libgambit.a -D___LIBRARY -D___SHARED

compile_scheme:
	${GSC} -:~~bin=${GSC_BIN},~~lib=${GSC_LIB},~~include=${GSC_INC} -f -link ${SCHEME_SOURCES}

clean:
	rm -f libDelectus.a
	rm -f libDelectus.dylib
	rm -f ${C_SOURCES}
	rm -f ${OBJECTS}
	rm -f *~

