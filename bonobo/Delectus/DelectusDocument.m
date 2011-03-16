//
//  DelectusDocument.m
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "DelectusDocument.h"
#import "DelectusDataSource.h"
#define ___VERSION 406000
#include "gambit.h"
#include "Delectus.h"

@implementation DelectusDocument

- (id)init
{
    self = [super init];
    if (self) {
    }
    return self;
}

- (NSString *)windowNibName
{
    return @"DelectusDocument";
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    [super windowControllerDidLoadNib:aController];
    if (dataSource==nil){
        dataSource=[[DelectusDataSource newDocument] retain];
    }
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
        viewFrame = [tableScrollView frame];
        viewFrame.size.height-=24;
        viewFrame.origin.y+=24;
        [tableScrollView setFrame:viewFrame];
        [tableScrollView setNeedsDisplay:YES];
    }else{
        [totalsView setHidden: YES];
        viewFrame = [tableScrollView frame];
        viewFrame.size.height+=24;
        viewFrame.origin.y-=24;
        [tableScrollView setFrame:viewFrame];
        [tableScrollView setNeedsDisplay:YES];
    }
}

- (IBAction)setFilter:(id)sender{}

- (IBAction)toggleShowDeleted:(id)sender{}

- (IBAction)emptyTrash:(id)sender{}

- (IBAction)renameColumn:(id)sender{}

// ----------------------------------------
// NSDocument APIs
// ----------------------------------------

- (BOOL)readFromURL: (NSURL *)absoluteURL ofType:(NSString *) typeName error: (NSError **)outError{
    NSDictionary* errDict;
    NSString *errStr, *errMsg;
    
    if ([typeName isEqualToString: @"csv"]) {
        errStr=@"CSVFormatError";
        errMsg=@"Couldn't read CSV data from the file";
        DelectusDataSource* src=[DelectusDataSource readDelectusCSV:absoluteURL];
        if (src==nil){
            errDict = [NSDictionary dictionaryWithObjectsAndKeys:errMsg, NSLocalizedDescriptionKey,[absoluteURL absoluteString], NSFilePathErrorKey, nil];
            *outError = [NSError errorWithDomain:errStr code:2 userInfo:errDict];
            return NO;
        }else{
            dataSource=[src retain];
            return YES;
        }
    } else if ([typeName isEqualToString: @"delectus"]) {
        errStr=@"DelectusFormatError";
        errMsg=@"Couldn't read Delectus data from the file";
        DelectusDataSource* src=[DelectusDataSource readDelectusFile:absoluteURL];
        if (src==nil){
            errDict = [NSDictionary dictionaryWithObjectsAndKeys:errMsg, NSLocalizedDescriptionKey,[absoluteURL absoluteString], NSFilePathErrorKey, nil];
            *outError = [NSError errorWithDomain:errStr code:2 userInfo:errDict];
            return NO;
        }else{
            dataSource=[src retain];
            return YES;
        }
    } else {
        errStr=@"FileFormatError";
        errMsg=@"Unrecognized file type";
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:errMsg, NSLocalizedDescriptionKey,[absoluteURL absoluteString], NSFilePathErrorKey, nil];
        *outError = [NSError errorWithDomain:errStr code:2 userInfo:errDict];
        return NO;
    }    
}

- (BOOL)writeDelectusToURL:(NSURL *)absoluteURL error:(NSError **)outError{
    NSDictionary* errDict;
    NSString* savePath = [absoluteURL path];
    char* path = (char*)[savePath cStringUsingEncoding: NSASCIIStringEncoding];
    int written_err = write_delectus_file([dataSource documentID], path);
    if(written_err == ERR_NO_ERROR){
        return YES;
    }else{
        NSString* errMsg = [NSString stringWithFormat: @"Write failed on pathname '%@'",savePath];
        NSRunAlertPanel(@"Save Failed",errMsg,nil, nil, nil);   
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:@"Save Failed", NSLocalizedDescriptionKey,savePath, NSFilePathErrorKey, nil];
        *outError = [NSError errorWithDomain:@"DelectusSaveError" code:3 userInfo:errDict];
        return NO;
    }
}

- (BOOL)writeCSVToURL:(NSURL *)absoluteURL error:(NSError **)outError{
    NSDictionary* errDict;
    NSString* savePath = [absoluteURL path];
    char* path = (char*)[savePath cStringUsingEncoding: NSASCIIStringEncoding];
    int written_err = write_delectus_csv([dataSource documentID], path);
    if(written_err == ERR_NO_ERROR){
        return YES;
    }else{
        NSString* errMsg = [NSString stringWithFormat: @"Write failed on pathname '%@'",savePath];
        NSRunAlertPanel(@"Save Failed",errMsg,nil, nil, nil);   
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:@"Save Failed", NSLocalizedDescriptionKey,savePath, NSFilePathErrorKey, nil];
        *outError = [NSError errorWithDomain:@"DelectusSaveError" code:3 userInfo:errDict];
        return NO;
    }
}


- (BOOL)writeToURL:(NSURL *)absoluteURL ofType:(NSString *)typeName error:(NSError **)outError{
    NSDictionary* errDict;
    if([typeName isEqualToString: @"delectus"]){
        BOOL result = [self writeDelectusToURL:absoluteURL error:outError];
        return result;
    }else if ([typeName isEqualToString: @"csv"]){
        BOOL result = [self writeCSVToURL:absoluteURL error:outError];
        return result;
    }else{
        NSString* errMsg = [NSString stringWithFormat: @"Can't save a document of type '%@'",typeName];
        NSRunAlertPanel(@"Save Failed",errMsg,nil, nil, nil);   
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:@"Save Failed", NSLocalizedDescriptionKey, nil];
        *outError = [NSError errorWithDomain:@"DelectusSaveError" code:3 userInfo:errDict];
        return NO;
    }
    return NO; // default--should never be reached
}


@end
