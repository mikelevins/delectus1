#!/bin/sh

export LISP='/Applications/LispWorks/LispWorks.app/Contents/MacOS/lispworks-5-0-0-macos-universal'

${LISP} -build deliver-cocoa.lisp 
