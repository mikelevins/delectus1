UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  GAMBIT_HOME=/usr/local
endif
ifeq ($(UNAME_S),Linux)
  GAMBIT_HOME=/usr/local/Gambit
endif

GCC = gcc

GSC=${GAMBIT_HOME}/bin/gsc
GSC_BIN=${GAMBIT_HOME}/bin
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib

SCHEME_SOURCES = ../engine/scm/base/constants.scm \
		 ../engine/scm/lib/uuid.scm \
		 ../engine/scm/lib/lists.scm \
		 ../engine/scm/lib/vectors.scm \
		 ../engine/scm/lib/strings.scm \
		 ../engine/scm/lib/Sort.scm \
		 ../engine/scm/lib/sort-keys.scm \
		 ../engine/scm/lib/filter-keys.scm \
		 ../engine/scm/lib/functions.scm \
		 ../engine/scm/data/entries.scm \
		 ../engine/scm/data/rows.scm \
		 ../engine/scm/data/columns.scm \
		 ../engine/scm/data/tables.scm \
		 ../engine/scm/data/views.scm \
		 ../engine/scm/data/registry.scm \
		 ../engine/scm/io/csv.scm \
		 ../engine/scm/io/io-formats.scm \
		 ../engine/scm/io/io.scm \
		 ../engine/scm/api/api.scm \
		 ../engine/scm/api/documents.scm \
                 ../engine/scm/api/engine.scm \
		 ../engine/scm/api/Delectus.scm

C_SOURCES =  ../engine/scm/base/constants.c \
		 ../engine/scm/lib/uuid.c \
		 ../engine/scm/lib/lists.c \
		 ../engine/scm/lib/vectors.c \
		 ../engine/scm/lib/strings.c \
		 ../engine/scm/lib/Sort.c \
		 ../engine/scm/lib/sort-keys.c \
		 ../engine/scm/lib/filter-keys.c \
		 ../engine/scm/lib/functions.c \
		 ../engine/scm/data/entries.c \
		 ../engine/scm/data/rows.c \
		 ../engine/scm/data/columns.c \
		 ../engine/scm/data/tables.c \
		 ../engine/scm/data/views.c \
		 ../engine/scm/data/registry.c \
		 ../engine/scm/io/csv.c \
		 ../engine/scm/io/io-formats.c \
		 ../engine/scm/io/io.c \
		 ../engine/scm/api/api.c \
		 ../engine/scm/api/documents.c \
                 ../engine/scm/api/engine.c \
		 ../engine/scm/api/Delectus.c \
		 ../engine/scm/api/Delectus_.c

OBJS = ../engine/scm/base/constants.o \
		 ../engine/scm/lib/uuid.o \
		 ../engine/scm/lib/lists.o \
		 ../engine/scm/lib/vectors.o \
		 ../engine/scm/lib/strings.o \
		 ../engine/scm/lib/Sort.o \
		 ../engine/scm/lib/sort-keys.o \
		 ../engine/scm/lib/filter-keys.o \
		 ../engine/scm/lib/functions.o \
		 ../engine/scm/data/entries.o \
		 ../engine/scm/data/rows.o \
		 ../engine/scm/data/columns.o \
		 ../engine/scm/data/tables.o \
		 ../engine/scm/data/views.o \
		 ../engine/scm/data/registry.o \
		 ../engine/scm/io/csv.o \
		 ../engine/scm/io/io-formats.o \
		 ../engine/scm/io/io.o \
		 ../engine/scm/api/api.o \
		 ../engine/scm/api/documents.o \
                 ../engine/scm/api/engine.o \
		 ../engine/scm/api/Delectus.o \
		 ../engine/scm/api/Delectus_.o


EXE = lecter

lecter:
	${GSC} -f -o ${EXE} -exe ${SCHEME_SOURCES} ./lecter.scm

clean:
	rm -f ${C_SOURCES}
	rm -f ${OBJS}
	rm -f src/*.o1
	rm -f src/*.o2
	rm -f *.o
	rm -f *.o1
	rm -f *.o2
	rm -f *~
