// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          NSString+CString.h
// Project:       Delectus
// Purpose:       NSString utils for use with Scheme
// Author:        mikel evins
//
// ***********************************************************************

#import "NSString+CString.h"
 
@implementation NSString ( CString )

-(const char*)asCString{
    [self cStringUsingEncoding: NSASCIIStringEncoding];
}


@end
