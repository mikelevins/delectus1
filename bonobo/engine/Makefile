SCM_SRCS= scm/constants.scm scm/api.scm
SCM_CSRCS= scm/constants.c scm/api.c scm/api_.c
SCM_OBJS= constants.o api.o api_.o

PRODUCT=libdelectus.a

GSC_INC=/Library/Gambit-C/current/include
GSC_LIB=/Library/Gambit-C/current/lib
CFLAGS= -I${GSC_INC} -L${GSC_LIB} -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp -arch i386 

all: lib

clean:
	rm -f *~
	rm -f scm/*~
	rm -f scm/*.c
	rm -f scm/*_.c

lib: obj
	ar rc libDelectus.a ${SCM_OBJS} && ranlib libDelectus.a
	rm *.o

obj: scheme 
	gcc ${CFLAGS} -c ${SCM_CSRCS} ${GSC_LIB}/libgambc.a -D_LIBRARY

scheme:
	gsc -:~~bin=${GSC_BIN},~~lib=${GSC_LIB},~~include=${GSC_INC} -f -link ${SCM_SRCS}


