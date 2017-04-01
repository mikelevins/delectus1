GAMBIT_HOME=/usr/local
GSC=${GAMBIT_HOME}/bin/gsc
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib

SCHEME_SOURCES = src/constants.scm \
		 src/lists.scm src/vectors.scm src/strings.scm \
                 src/Sort.scm src/sort-keys.scm src/filter-keys.scm \
		 src/functions.scm \
	         src/api.scm \
                 src/entries.scm src/rows.scm src/columns.scm \
                 src/tables.scm src/views.scm src/registry.scm \
                 src/engine.scm \
                 src/csv.scm src/io-formats.scm src/io.scm  \
                 src/lecter.scm

PRODUCT = lecter

FLAGS_OBJ = -I${GSC_INC} -L${GSC_LIB} -no-cpp-precomp -Wno-unused -O1 -fno-math-errno -fschedule-insns2 -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp 

compile:
	${GSC} -f -o ${PRODUCT} -exe ${SCHEME_SOURCES}

clean:
	rm -f ${PRODUCT}
	rm -f *~
