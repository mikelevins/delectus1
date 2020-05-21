#!/bin/bash

# link the lispworks executable to a sane Unix pathname to make this work
LISP="/usr/local/bin/lispworks"
DELIVER_SCRIPT="/Users/mikel/Workshop/src/delectus/product/macos/deliver.lisp"

${LISP} -build ${DELIVER_SCRIPT}
