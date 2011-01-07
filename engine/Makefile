GAMBIT_HOME=/usr/local/gambit/MacOSX
GSC=${GAMBIT_HOME}/bin/gsc
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib
GCC = gcc

FLAGS_OBJ = -I${GSC_INC} -L${GSC_LIB} -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp 

FLAGS_LIB =  -I${GSC_INC} -L${GSC_LIB} -flat_namespace -undefined suppress

FLAGS_EXE =  -I${GSC_INC} -L${GSC_LIB} -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp  


exe: lib
	${GCC} ${FLAGS_EXE} -I"${GSC_INC}" src/delectus_test.c -o delectus_test -L${GSC_LIB} -L. -ldelectus -lgambc

lib: obj
	ar rc libdelectus.a api.o delectus.o delectus_.o && ranlib libdelectus.a

obj: compile_scheme
	${GCC} ${FLAGS_OBJ} -I"${GSC_INC}" -c src/api.c src/delectus.c src/delectus_.c  -D___LIBRARY

compile_scheme:
	${GSC} -:~~bin=${GSC_BIN},~~lib=${GSC_LIB},~~include=${GSC_INC} -f -link src/api.scm src/delectus.scm

clean:
	rm -f src/api.c src/delectus.c src/delectus_.c libdelectus.a delectus_test
	rm -f *.o
	rm -f *~
