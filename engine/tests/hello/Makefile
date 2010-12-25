GAMBIT_HOME=/usr/local/gambit/MacOSX
GSC=${GAMBIT_HOME}/bin/gsc
C_COMPILER = gcc
C_PREPROC = gcc -E
FLAGS_OBJ = -I/Users/mikel/Attic/Software/Scheme/impl/gambit/gambit-macosx/current/include -L/Users/mikel/Attic/Software/Scheme/impl/gambit/gambit-macosx/current/lib   -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp 
FLAGS_DYN =  -I/Users/mikel/Attic/Software/Scheme/impl/gambit/gambit-macosx/current/include -L/Users/mikel/Attic/Software/Scheme/impl/gambit/gambit-macosx/current/lib  -bundle  -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp   -flat_namespace -undefined suppress
FLAGS_LIB =  -I/Users/mikel/Attic/Software/Scheme/impl/gambit/gambit-macosx/current/include -L/Users/mikel/Attic/Software/Scheme/impl/gambit/gambit-macosx/current/lib  -dynamiclib -install_name $(libdir)/$(LIBRARY)   -flat_namespace -undefined suppress
FLAGS_EXE =  -I/Users/mikel/Attic/Software/Scheme/impl/gambit/gambit-macosx/current/include -L/Users/mikel/Attic/Software/Scheme/impl/gambit/gambit-macosx/current/lib   -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp  
DEFS = -DHAVE_CONFIG_H
LIBS = 

GAMBCDIR_INCLUDE = ${GAMBIT_HOME}/include
GAMBCDIR_LIB = ${GAMBIT_HOME}/lib
GAMBCDIR_LIB_GSI = ${GAMBIT_HOME}/gsi
GAMBCDIR_LIB_GSC = ${GAMBIT_HOME}/gsc

all:
	 ${GSC} -:~~bin=${GAMBIT_HOME}/bin,~~lib=${GAMBIT_HOME}/lib,~~include=${GAMBIT_HOME}/include -f -link hello_server.scm
	$(MAKE) build_obj \
	  BUILD_OBJ_OUTPUT_FILENAME="hello.o" \
	  BUILD_OBJ_CC_OPTIONS="" \
	  BUILD_OBJ_INPUT_FILENAMES="hello.c"
	$(MAKE) build_obj_library \
	  BUILD_OBJ_OUTPUT_FILENAME="hello_server.o" \
	  BUILD_OBJ_CC_OPTIONS="" \
	  BUILD_OBJ_INPUT_FILENAMES="hello_server.c"
	$(MAKE) build_obj_library \
	  BUILD_OBJ_OUTPUT_FILENAME="hello_server_.o" \
	  BUILD_OBJ_CC_OPTIONS="" \
	  BUILD_OBJ_INPUT_FILENAMES="hello_server_.c"
	$(MAKE) build_exe \
	  BUILD_EXE_OUTPUT_FILENAME="hello" \
	  BUILD_EXE_CC_OPTIONS="" \
	  BUILD_EXE_LD_OPTIONS_PRELUDE="" \
	  BUILD_EXE_INPUT_FILENAMES="hello.o hello_server.o hello_server_.o" \
	  BUILD_EXE_LD_OPTIONS=""

build_obj:
	${C_COMPILER} ${FLAGS_OBJ} ${DEFS_OBJ} -I"${GAMBCDIR_INCLUDE}" -c -o "${BUILD_OBJ_OUTPUT_FILENAME}" ${BUILD_OBJ_CC_OPTIONS} ${BUILD_OBJ_INPUT_FILENAMES}

build_obj_library:
	${C_COMPILER} ${FLAGS_OBJ} ${DEFS_OBJ} -I"${GAMBCDIR_INCLUDE}" -c -o "${BUILD_OBJ_OUTPUT_FILENAME}" ${BUILD_OBJ_CC_OPTIONS} ${BUILD_OBJ_INPUT_FILENAMES} -D___LIBRARY

build_exe:
	${C_COMPILER} ${FLAGS_EXE} ${DEFS_EXE} -I"${GAMBCDIR_INCLUDE}"  -o "${BUILD_EXE_OUTPUT_FILENAME}" ${BUILD_EXE_CC_OPTIONS} ${BUILD_EXE_LD_OPTIONS_PRELUDE} ${BUILD_EXE_INPUT_FILENAMES} "${GAMBCDIR_LIB}/libgambc.a" ${LIBS} ${BUILD_EXE_LD_OPTIONS}

clean:
	rm -f hello_server.c hello_server_.c hello_server.o hello_server_.o hello.o hello
	rm *~
