#!/bin/sh

sbcl --no-userinit --load lisp/Delectus2.asd --eval "(cl-user::buildapp)"

# Package
npm i
electron-packager . $APP_NAME --overwrite --icon=./../../assets/images/app.icns --ignore="\.gitignore|.*\.sh|lisp|README\.adoc"
