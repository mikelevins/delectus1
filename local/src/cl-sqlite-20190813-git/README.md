# Local cl-sqlite

Modified sqlite-ffi to defer loading the sqlite 3 library.

Exported the new macro INIT-SQLITE-FFI, which accepts a
loadable-library pathname and then calls DEFINE-FOREIGN-LIBRARY and
USE-FOREIGN-LIBRARY using that pathname as the library to load.

Using this local modification, we can load cl-sqlite at load and build
time, and separately call SQLITE-FFI::INIT-SQLITE-FFI to actually load
and use the library later (for example, at application startup
time). We can also execute some code to find the proper library file
depending on whether we're loading it from the development environment
or the delivered application.
