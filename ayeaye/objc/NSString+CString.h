// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          NSString+CString.h
// Project:       Delectus
// Purpose:       NSString utils for use with Scheme
// Author:        mikel evins
//
// ***********************************************************************

#import <Foundation/NSString.h>
 
@interface NSString (CString)

-(const char*)asCString;

@end
