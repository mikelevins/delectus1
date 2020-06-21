#!/bin/bash

# link the lispworks executable to a sane Unix pathname to make this work
LISP="/usr/local/bin/lispworks"
DELIVER_SCRIPT="/Users/mikel/Workshop/src/delectus/platform/macos/deliver_capi.lisp"

${LISP} -build ${DELIVER_SCRIPT}
