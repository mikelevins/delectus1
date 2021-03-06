UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  GAMBIT_HOME=/usr/local/Gambit
endif
ifeq ($(UNAME_S),Linux)
  GAMBIT_HOME=/usr/local/Gambit
endif

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

OBJS = scm/base/constants.o \
       scm/lib/lists.o scm/lib/vectors.o scm/lib/strings.o \
       scm/lib/Sort.o scm/lib/sort-keys.o scm/lib/filter-keys.o \
       scm/lib/functions.o \
       scm/api/api.o \
       scm/data/entries.o scm/data/rows.o scm/data/columns.o \
       scm/data/tables.o scm/data/views.o scm/data/registry.o \
       scm/api/engine.o \
       scm/io/csv.o scm/io/io-formats.o scm/io/io.o  \
       scm/api/Delectus.o scm/api/Delectus_.o

LIB = libDelectus.a

lib: obj
	ar rc ${LIB} ${OBJS} && ranlib ${LIB}
	rm -f ${C_SOURCES}
	rm -f ${OBJECTS}

obj: compile_scheme
	${GSC} -obj -cc-options "-D___LIBRARY -mmacosx-version-min=10.12" ${C_SOURCES}

compile_scheme:
	${GSC} -link ${SCHEME_SOURCES}

clean:
	rm -f ${C_SOURCES}
	rm -f ${OBJS}
	rm -f src/*.o1
	rm -f src/*.o2
	rm -f *.o
	rm -f *.o1
	rm -f *.o2
	rm -f *~

