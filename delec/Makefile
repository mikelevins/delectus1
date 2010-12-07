EXECUTABLE=delec

SCM_LIBS=lib/scm/csv.scm lib/scm/Sort.scm lib/srfi/srfi1.scm lib/srfi/srfi13.scm lib/srfi/srfi28.scm
SCM_LIBS_CFILES=lib/scm/csv.c lib/scm/Sort.c lib/srfi/srfi1.c lib/srfi/srfi13.c lib/srfi/srfi28.c

SCM_SRCS=  scm/main.scm 
SCM_SRCS_CFILES=  scm/main.c scm/main_.c

SCM_CFILES= ${SCM_LIBS_CFILES} ${SCM_SRCS_CFILES}

CFLAGS= -O2 -I/Library/Gambit-C/current/include -L/Library/Gambit-C/current/lib -arch ppc -arch i386
CLIBS= -lgambc

all: delec

clean:
	rm -f ${SCM_LIBS_CFILES} ${SCM_SRCS_CFILES}
	rm -f ${EXECUTABLE}
	rm -f *~

tidy:
	rm -f ${SCM_LIBS_CFILES} ${SCM_SRCS_CFILES}
	rm -f ${EXECUTABLE}
	rm -f *~


delec: scheme 
	gcc ${CFLAGS} -o ${EXECUTABLE} ${CLIBS} ${SCM_LIBS_CFILES} ${SCM_SRCS_CFILES}

scheme:
	gsc -link ${SCM_LIBS} ${SCM_SRCS}


