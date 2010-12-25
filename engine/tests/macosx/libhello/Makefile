GAMBIT_HOME=/usr/local/gambit/MacOSX
GSC=${GAMBIT_HOME}/bin/gsc
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib
GCC = gcc

FLAGS_OBJ = -I${GSC_INC} -L${GSC_LIB} -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp 

FLAGS_LIB =  -I${GSC_INC} -L${GSC_LIB} -flat_namespace -undefined suppress

lib: obj
	ar rc libhello.a hello.o hello_.o && ranlib libhello.a

obj: compile_scheme
	${GCC} ${FLAGS_OBJ} -I"${GSC_INC}" -c hello.c hello_.c ${GSC_LIB}/libgambc.a  -D___LIBRARY

compile_scheme:
	${GSC} -:~~bin=${GSC_BIN},~~lib=${GSC_LIB},~~include=${GSC_INC} -f -link hello.scm

clean:
	rm -f hello.c hello_.c hello.o hello_.o greet.o greet
	rm -f *~
