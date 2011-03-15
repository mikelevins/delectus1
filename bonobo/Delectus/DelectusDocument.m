//
//  DelectusDocument.m
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "DelectusDocument.h"
#define ___VERSION 406000
#include "gambit.h"
#include "Delectus.h"

@implementation DelectusDocument

- (id)init
{
    self = [super init];
    if (self) {
    
        // Add your subclass-specific initialization here.
        // If an error occurs here, send a [self release] message and return nil.
    
    }
    return self;
}

- (NSString *)windowNibName
{
    // Override returning the nib file name of the document
    // If you need to use a subclass of NSWindowController or if your document supports multiple NSWindowControllers, you should remove this method and override -makeWindowControllers instead.
    return @"DelectusDocument";
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    // how to get and release a string value
    //char* versionString=version();
    //NSLog(@"\nversion = %s\n\n", versionString);
    //NSString* vStr = [[NSString stringWithCString:versionString] retain];
    //___release_string(versionString);
    //NSRunAlertPanel(@"version test",vStr,@"Okay",nil,nil);
    [super windowControllerDidLoadNib:aController];
}

// IBActions
- (IBAction)toggleToDo:(id)sender{}

- (IBAction)addRow:(id)sender{}

- (IBAction)toggleRowDeleted:(id)sender{}

- (IBAction)addColumn:(id)sender{}

- (IBAction)toggleColumnDeleted:(id)sender{}

- (IBAction)toggleTotals:(id)sender{
    NSInteger state = [sender state];
    NSRect viewFrame;
    if (state == NSOnState){
        [totalsView setHidden: NO];
        [totalsView setNeedsDisplay:YES];
        viewFrame = [tableView frame];
        viewFrame.size.height-=24;
        viewFrame.origin.y+=24;
        [tableView setFrame:viewFrame];
        [tableView setNeedsDisplay:YES];
    }else{
        [totalsView setHidden: YES];
        viewFrame = [tableView frame];
        viewFrame.size.height+=24;
        viewFrame.origin.y-=24;
        [tableView setFrame:viewFrame];
        [tableView setNeedsDisplay:YES];
    }
}

- (IBAction)setFilter:(id)sender{}

- (IBAction)toggleShowDeleted:(id)sender{}

- (IBAction)emptyTrash:(id)sender{}

- (IBAction)renameColumn:(id)sender{}

// ----------------------------------------
// NSDocument APIs
// ----------------------------------------

- (int)readCSVFile:(NSURL *)absoluteURL {
    NSString* fPath = [absoluteURL path];
	char* path = (char*)[fPath cStringUsingEncoding: NSASCIIStringEncoding];
    int docid = read_delectus_csv(path);
    return docid;
}

- (int)readDelectusFile:(NSURL *)absoluteURL {
    NSString* fPath = [absoluteURL path];
	char* path = (char*)[fPath cStringUsingEncoding: NSASCIIStringEncoding];
    int docid = read_delectus_file(path);
    NSLog(@"readDelectusFile docid == %d",docid);
    return docid;
}

- (BOOL)readFromURL: (NSURL *)absoluteURL ofType:(NSString *) typeName error: (NSError **)outError{
    NSString* fPath = [absoluteURL path];
    NSDictionary* errDict;
    NSString *errStr, *errMsg;
    
    if ([typeName isEqualToString: @"csv"]) {
        errStr=@"CSVFormatError";
        errMsg=@"Couldn't read CSV data from the file";
        documentID = [self readCSVFile:absoluteURL];
    } else if ([typeName isEqualToString: @"delectus"]) {
        errStr=@"DelectusFormatError";
        errMsg=@"Couldn't read Delectus data from the file";
        documentID = [self readDelectusFile:absoluteURL];
    } else {
        errStr=@"FileFormatError";
        errMsg=@"Unrecognized file type";
        documentID=OBJ_NO_OID;
    }
    
    if (documentID != OBJ_NO_OID){
        return YES;
    }else{
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:
                   errMsg, NSLocalizedDescriptionKey,
                   fPath, NSFilePathErrorKey, nil
                   ];
        *outError = [NSError errorWithDomain:errStr code:2 userInfo:errDict];
        return NO;
    }
}

- (BOOL)writeToURL:(NSURL *)absoluteURL ofType:(NSString *)typeName error:(NSError **)outError{
    NSDictionary* errDict;
    NSString* savePath;
    if([typeName isEqualToString: @"delectus"]){
        savePath = [absoluteURL path];
        int written_err = write_delectus_file(documentID, [savePath asCString]);
        if(written_err == ERR_NO_ERROR){
            return YES;
        }else{
            NSString* errMsg = [NSString stringWithFormat: @"Write failed on pathname '%@'",savePath];
            NSRunAlertPanel(@"Save Failed",errMsg,nil, nil, nil);   
            errDict = [NSDictionary dictionaryWithObjectsAndKeys:
                       @"Save Failed", NSLocalizedDescriptionKey,
                       savePath, NSFilePathErrorKey, nil];
            *outError = [NSError errorWithDomain:@"DelectusSaveError" code:3 userInfo:errDict];
            return NO;
        }
    }else if ([typeName isEqualToString: @"csv"]){
        savePath = [absoluteURL path];
        int written_err = write_delectus_csv(documentID, [savePath asCString]);
        if(written_err == ERR_NO_ERROR){
            return YES;
        }else{
            NSString* errMsg = [NSString stringWithFormat: @"Write failed on pathname '%@'",savePath];
            NSRunAlertPanel(@"Save Failed",errMsg,nil, nil, nil);   
            errDict = [NSDictionary dictionaryWithObjectsAndKeys:
                       @"Save Failed", NSLocalizedDescriptionKey,
                       savePath, NSFilePathErrorKey, nil];
            *outError = [NSError errorWithDomain:@"DelectusSaveError" code:3 userInfo:errDict];
            return NO;}
    }else{
        NSString* errMsg = [NSString stringWithFormat: @"Can't save a document of type '%@'",typeName];
        NSRunAlertPanel(@"Save Failed",errMsg,nil, nil, nil);   
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:
                   @"Save Failed", NSLocalizedDescriptionKey, nil];
        *outError = [NSError errorWithDomain:@"DelectusSaveError" code:3 userInfo:errDict];
        return NO;
    }
}



@end
