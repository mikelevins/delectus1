#!/bin/sh

sbcl --no-userinit --load lisp/delectus2.asd --eval "(cl-user::buildapp)"
