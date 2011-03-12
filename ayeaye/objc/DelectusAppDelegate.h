// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          DelectusAppDelegate.h
// Project:       Delectus
// Purpose:       App Delegate definitions
// Author:        mikel evins
//
// ***********************************************************************

#import <Cocoa/Cocoa.h>

@class DelectusDocument;

@interface DelectusAppDelegate : NSObject {
    IBOutlet id emptyTrashCell;
    IBOutlet id fullTrashCell;
}

-(void)applicationDidFinishLaunching:(NSNotification *)aNotification;
-(void) schemeError:(NSNotification*)notification;
-(DelectusDocument*)documentForID: (int)docID;
-(void) reloadData:(NSNotification*)notification;
-(NSImage*) trashEmptyImage;
-(NSImage*) trashFullImage;

@end
