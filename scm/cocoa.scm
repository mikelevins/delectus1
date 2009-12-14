;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Cocoa.scm
;;;; Project:       Delectus
;;;; Purpose:       Scheme wrappers for commonly-called Objective-C
;;;;                Cocoa operations
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; About
;;; ----------------------------------------------------------------------
;;; Functions in this file are called from Scheme, but are implemented
;;; as Objective-C functions, and return Objective-C values

(c-declare "#include <Cocoa/Cocoa.h>")
(c-declare "#include <AppKit/AppKit.h>")

;;; ----------------------------------------------------------------------
;;; Cocoa APIs
;;; ----------------------------------------------------------------------

(define cocoa:application-main
  (c-lambda () int
#<<c-code
   NSApplication *app = [NSApplication sharedApplication];
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
   [NSBundle loadNibNamed: [[[NSBundle mainBundle] infoDictionary] objectForKey: @"NSMainNibFile"]
             owner: app];
   [app run];
   [pool release];
   ___result = 0;
c-code
))

(define cocoa:null
  (c-lambda () (pointer "void")
#<<c-code
   ___result_voidstar = (void*)NULL;
   
c-code
))

(define cocoa:make-ns-string
  (c-lambda (char-string) (pointer "NSString")
#<<c-code
   NSString* s = (NSString*)[NSString stringWithCString:___arg1 encoding:NSASCIIStringEncoding];
   [s retain];
   ___result_voidstar = (void*)s;
   
c-code
))

(define cocoa:make-ns-mutable-array
  (c-lambda () (pointer "NSMutableArray")
#<<c-code
   NSMutableArray* arr = [NSMutableArray array];
   [arr retain];
   ___result_voidstar = (void*)arr;
c-code
))

(define cocoa:make-ns-mutable-dictionary
  (c-lambda () (pointer "NSMutableDictionary")
#<<c-code
   NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithCapacity:16];
   [dict retain];
   ___result_voidstar = (void*)dict;
c-code
))

(define cocoa:set-key/string
  (c-lambda ((pointer "NSMutableDictionary") char-string char-string) void
#<<c-code
   NSMutableDictionary* dict =___arg1;
   NSString* key = [NSString stringWithCString: ___arg2 encoding: NSASCIIStringEncoding];
   NSString* val = [NSString stringWithCString: ___arg3 encoding: NSASCIIStringEncoding];
   [dict setObject: val forKey:key];
c-code
))


(define cocoa:add-integer
  (c-lambda ((pointer "NSMutableArray") int) (pointer "NSMutableArray")
#<<c-code
   [___arg1 addObject: [NSNumber numberWithInt: ___arg2]];
   ___result_voidstar = (void*)___arg1;
c-code
))

(define cocoa:add-ns-string
  (c-lambda ((pointer "NSMutableArray")(pointer "NSString")) (pointer "NSMutableArray")
#<<c-code
   [___arg1 addObject: ___arg2];
   ___result_voidstar = (void*)___arg1;
c-code
))

(define cocoa:notify-error
  (c-lambda (char-string) void
#<<c-code
   char* msg=___arg1;
   NSString* message = [NSString stringWithCString:msg encoding: NSASCIIStringEncoding];
   [[NSNotificationCenter defaultCenter] postNotificationName: @"SchemeError" object:message];
c-code
))

(define cocoa:notify-reload-data
  (c-lambda (int) void
#<<c-code
   int docID=___arg1;
   [[NSNotificationCenter defaultCenter] postNotificationName: @"ReloadData" 
    object:[NSNumber numberWithInt: docID]];
c-code
))

(define cocoa:notify-deselect-all
  (c-lambda (int) void
#<<c-code
   int docID=___arg1;
   [[NSNotificationCenter defaultCenter] postNotificationName: @"DeselectAll" 
    object:[NSNumber numberWithInt: docID]];
c-code
))

(define cocoa:release
  (c-lambda ((pointer "NSObject")) void
#<<c-code
   [___arg1 release];
c-code
))

