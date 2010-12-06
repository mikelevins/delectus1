
// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          DelectusAppDelegate.h
// Project:       Delectus
// Purpose:       App Delegate implementation
// Author:        mikel evins
//
// ***********************************************************************

#import "DelectusAppDelegate.h"
#import "DelectusDocument.h"

@implementation DelectusAppDelegate

-(NSImage*) trashEmptyImage {return [emptyTrashCell image];}
-(NSImage*) trashFullImage {return [fullTrashCell image];}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification{
    NSNotificationCenter* notificationCenter = [NSNotificationCenter defaultCenter];
    [notificationCenter addObserver:self selector:@selector(schemeError:) name:@"SchemeError" object:nil];
    [notificationCenter addObserver:self selector:@selector(reloadData:) name:@"ReloadData" object:nil];
    [notificationCenter addObserver:self selector:@selector(deselectAll:) name:@"DeselectAll" object:nil];
}

- (void) schemeError:(NSNotification*)notification {
    NSString* errMessage = (NSString*)[notification object];
    NSRunAlertPanel(@"Alert",errMessage,nil, nil, nil);   
}

- (DelectusDocument*)documentForID: (int)docID{
    if(docID==NO_DOCUMENT){
        return NULL;
    }else{
        int i;
        NSArray* docs = [[NSDocumentController sharedDocumentController] documents];
        for(i=0;i<[docs count];i++){
            DelectusDocument* doc = (DelectusDocument*)[docs objectAtIndex:i];
            int did = [doc documentID];
            if (did == docID){return doc;}
        }
        return NULL;
    }
}

- (void) reloadData:(NSNotification*)notification {
    NSNumber* document_ID = (NSNumber*)[notification object];
    int docID = [document_ID intValue];
    NSDocumentController* docController = [NSDocumentController sharedDocumentController];
    DelectusDocument* doc = (DelectusDocument*)[self documentForID: docID];
    if(doc == NULL){
        NSLog(@"in reloadData: no document for id '%@'",docID);
    }else{
        [doc reloadData];
    }
}

- (void) deselectAll:(NSNotification*)notification {
    NSNumber* document_ID = (NSNumber*)[notification object];
    int docID = [document_ID intValue];
    NSDocumentController* docController = [NSDocumentController sharedDocumentController];
    DelectusDocument* doc = (DelectusDocument*)[self documentForID: docID];
    if(doc == NULL){
        NSLog(@"in deselectAll: no document for id '%@'",docID);
    }else{
        [doc deselectAll];
    }
}

@end
