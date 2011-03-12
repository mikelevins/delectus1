GAMBIT_HOME=/usr/local/gambit/MacOSX
GSC=${GAMBIT_HOME}/bin/gsc
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib
GCC = gcc

SCHEME_SOURCES = scm/constants.scm scm/api.scm scm/Delectus.scm
C_SOURCES = scm/constants.c scm/api.c scm/Delectus.c scm/Delectus_.c
OBJECTS = constants.o api.o Delectus.o Delectus_.o
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
