APPNAME=Delectus
BUNDLE=${APPNAME}.app

SCHEME_LIBRARY_PATH=/usr/local/slib/

SCM_LIBS=lib/scm/csv.scm lib/scm/Sort.scm lib/srfi/srfi1.scm lib/srfi/srfi13.scm lib/srfi/srfi28.scm
SCM_LIBS_CFILES=lib/scm/csv.c lib/scm/Sort.c lib/srfi/srfi1.c lib/srfi/srfi13.c lib/srfi/srfi28.c

SCM_SRCS= scm/utils.scm scm/errors.scm scm/store.scm scm/document.scm scm/csv-utils.scm scm/store-io.scm scm/document-io.scm scm/cocoa.scm scm/bridge.scm scm/api.scm scm/c-interface.scm scm/main.scm 
SCM_SRCS_CFILES= scm/utils.c scm/errors.c scm/store.c scm/document.c scm/csv-utils.c scm/store-io.c scm/document-io.c scm/cocoa.c scm/bridge.c scm/api.c scm/c-interface.c scm/main.c scm/main_.c

SCM_CFILES= ${SCM_LIBS_CFILES} ${SCM_SRCS_CFILES}

OBJC_SRCS= objc/DelectusAppDelegate.m objc/NSString+CString.m objc/DelectusDocument.m objc/DelectusCell.m objc/DelectusTableView.m 


EXECUTABLE=${APPNAME}.out

CFLAGS= -O2 -x objective-c -I/Library/Gambit-C/current/include -L/Library/Gambit-C/current/lib -framework Cocoa -arch ppc -arch i386
CLIBS= -lgambc

all: bundle

clean:
	rm -rf ${BUNDLE}
	rm -f ${SCM_LIBS_CFILES} ${SCM_SRCS_CFILES}
	rm -f ${EXECUTABLE}
	rm -f *~

tidy:
	rm -f ${SCM_LIBS_CFILES} ${SCM_SRCS_CFILES}
	rm -f ${EXECUTABLE}
	rm -f *~


bundle: cocoa
	mkdir -p ./${BUNDLE}/Contents/Resources/English.lproj/
	mkdir -p ./${BUNDLE}/Contents/MacOS
	ibtool ./Contents/Resources/English.lproj/MainMenu.xib --compile ./${BUNDLE}/Contents/Resources/English.lproj/MainMenu.nib
	ibtool ./Contents/Resources/English.lproj/DelectusDocument.xib --compile ./${BUNDLE}/Contents/Resources/English.lproj/DelectusDocument.nib
	rsync -r --exclude '.svn' --exclude '*.xib' ./Contents ./${BUNDLE}
	mv ./${EXECUTABLE} ./${BUNDLE}/Contents/MacOS/${APPNAME}
	rm -f ${SCM_LIBS_CFILES} ${SCM_SRCS_CFILES}

cocoa: scheme 
	gcc ${CFLAGS} -o ${EXECUTABLE} ${CLIBS} ${SCM_LIBS_CFILES} ${OBJC_SRCS} ${SCM_SRCS_CFILES}

scheme:
	gsc -link ${SCM_LIBS} ${SCM_SRCS}


