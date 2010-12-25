//
//  HelloDelegate.m
//  Hello
//
//  Created by mikel on 12/25/10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import "HelloDelegate.h"
#include "hello.h"

@implementation HelloDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification{
	char* greeting = hello(" Cocoa");
	NSString* msg = [NSString stringWithCString:greeting];
	NSRunAlertPanel(@"greeting", msg, nil, nil, nil);
}

@end
