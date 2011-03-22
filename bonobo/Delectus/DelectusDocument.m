//
//  DelectusDocument.m
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "DelectusDocument.h"
#import "DelectusDelegate.h"
#import "DelectusDataSource.h"
#define ___VERSION 406000
#include "gambit.h"
#include "Delectus.h"

@implementation DelectusDocument

// --------------------------------------------------------------------------------
// init
// --------------------------------------------------------------------------------

- (id)init
{
    self = [super init];
    if (self) {
        contentFont=[NSFont systemFontOfSize:12.0];
        filterText=nil;
    }
    return self;
}

// --------------------------------------------------------------------------------
// Window init
// --------------------------------------------------------------------------------

- (NSString *)windowNibName
{
    return @"DelectusDocument";
}

- (void)setupSpacing{
    // tableview setup
    NSSize spacing;
    spacing.width=1.0;
    spacing.height=0.0;
    [tableView setIntercellSpacing:spacing];
}

- (void)setupColumns{
    [self setupSpacing];
    [tableView setAutosaveName:nil];
    [tableView setAutosaveTableColumns:NO];
    NSArray* columnLabels = [dataSource collectColumns];
    NSFont* headerFont = [NSFont systemFontOfSize:12.0];
    int colcount = [columnLabels count];
    for(int i = 0;i<colcount;i++){
        NSString* label = (NSString*)[columnLabels objectAtIndex:i];
        NSTableColumn* col = [[NSTableColumn alloc] initWithIdentifier: label];
        [col retain];
        [[col headerCell] setStringValue: label];
        [[col headerCell] setFont:headerFont];
        [[col dataCell] setFont:contentFont];
        [tableView addTableColumn: col];
    }
    [tableView setAutosaveName:@"delectus1.0"];
    [tableView setAutosaveTableColumns:YES];
}

- (void)discardColumns{
    NSArray* cols=[tableView tableColumns];
    int colcount = [cols count];
    for(int i = 0;i<colcount;i++){
        NSTableColumn* col = [cols objectAtIndex:i];
        [tableView removeTableColumn: col];
        [col release];
    }
}

- (void)updateDisplay{
    int rowCount = [tableView numberOfRows];
    [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",rowCount]];
    [tableView setNeedsDisplay:YES];
    [itemCountField setNeedsDisplay:YES];
}

- (void)updateDataView{
    BOOL include_deleted = NO; // TODO: fetch this from user-supplied state
    NSString* sort_column = nil; // TODO: fetch this from user-supplied state
    int sort_order = SORT_NONE;  // TODO: fetch this from user-supplied state
    [dataSource getViewIncludingDeleted:include_deleted withSortColumn:sort_column andSortOrder:sort_order andFilterText:filterText];
    [tableView reloadData];
    [self updateDisplay];
}

- (void)rebuildDisplay{
    [self discardColumns];
    [self setupColumns];
    [self updateDataView];
    [self updateDisplay];
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    [super windowControllerDidLoadNib:aController];
    if (dataSource==nil){
        dataSource=[[[NSApp delegate] newDelectus] retain];
    }
    [tableView setDataSource: dataSource];
    [tableView setDelegate: self];
    [addColumnButton setTarget:self];
    [delColumnButton setTarget:self];
    [addRowButton setTarget: self];
    [delRowButton setTarget: self];
    [showDeletedButton setTarget: self];
    [self rebuildDisplay];
    
}

// --------------------------------------------------------------------------------
// IBActions
// --------------------------------------------------------------------------------

- (IBAction)addRow:(id)sender{}

- (IBAction)toggleRowDeleted:(id)sender{}

- (IBAction)addColumn:(id)sender{}

- (IBAction)toggleColumnDeleted:(id)sender{}

- (IBAction)toggleShowDeleted:(id)sender{}

- (IBAction)emptyTrash:(id)sender{}

- (IBAction)renameColumn:(id)sender{}

- (IBAction)setFilter:(id)sender{
    NSSearchField* searchField = (NSSearchField*)sender;
    filterText = [[searchField cell] stringValue];
    [self updateDataView];
}


// --------------------------------------------------------------------------------
// Handle font changes
// --------------------------------------------------------------------------------

- (NSFont*)font{
    return contentFont;
}

- (void)setFont:(NSFont*)newFont{
    contentFont = newFont;
    NSArray* cols = [tableView tableColumns];
    int colcount = [cols count];
    for(int i = 0;i<colcount;i++){
        NSTableColumn* col = [cols objectAtIndex:i];
        [[col dataCell] setFont:newFont];
    }
    [tableView setRowHeight:[newFont pointSize]*1.8];
    [tableView setNeedsDisplay:YES];
}

- (void)changeFont:(id)sender
{    
    NSFont *oldFont = [self font];
    NSFont *newFont = [sender convertFont:oldFont];
    [self setFont:newFont];
    return;

}


// --------------------------------------------------------------------------------
// NSDocument APIs
// --------------------------------------------------------------------------------

- (BOOL)readFromURL: (NSURL *)absoluteURL ofType:(NSString *) typeName error: (NSError **)outError{
    NSDictionary* errDict;
    NSString *errStr, *errMsg;
    
    if ([typeName isEqualToString: @"csv"]) {
        errStr=@"CSVFormatError";
        errMsg=@"Couldn't read CSV data from the file";
        DelectusDataSource* src=[[NSApp delegate] readCSVFile:absoluteURL];
        if (src==nil){
            errDict = [NSDictionary dictionaryWithObjectsAndKeys:errMsg, NSLocalizedDescriptionKey,[absoluteURL path], NSFilePathErrorKey, nil];
            *outError = [[NSError errorWithDomain:errStr code:2 userInfo:errDict] autorelease];
            return NO;
        }else{
            dataSource=[src retain];
            return YES;
        }
    } else if ([typeName isEqualToString: @"delectus"]) {
        errStr=@"DelectusFormatError";
        errMsg=@"Couldn't read Delectus data from the file";
        DelectusDataSource* src=[[NSApp delegate] readDelectusFile:absoluteURL];
        if (src==nil){
            errDict = [NSDictionary dictionaryWithObjectsAndKeys:errMsg, NSLocalizedDescriptionKey,[absoluteURL path], NSFilePathErrorKey, nil];
            *outError = [[NSError errorWithDomain:errStr code:2 userInfo:errDict] autorelease];
            return NO;
        }else{
            dataSource=[src retain];
            return YES;
        }
    } else {
        errStr=@"FileFormatError";
        errMsg=@"Unrecognized file type";
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:errMsg, NSLocalizedDescriptionKey,[absoluteURL path], NSFilePathErrorKey, nil];
        *outError = [[NSError errorWithDomain:errStr code:2 userInfo:errDict] autorelease];
        return NO;
    }    
}

- (BOOL)writeDelectusToURL:(NSURL *)absoluteURL error:(NSError **)outError{
    NSDictionary* errDict;
    int result = [dataSource writeDelectusFile:absoluteURL];
    if(result == ERR_NO_ERROR){
        return YES;
    }else{
         errDict = [NSDictionary dictionaryWithObjectsAndKeys:@"Save Failed", NSLocalizedDescriptionKey,[absoluteURL path], NSFilePathErrorKey, nil];
        *outError = [[NSError errorWithDomain:@"DelectusSaveError" code:result userInfo:errDict] autorelease];
        return NO;
    }
}



- (BOOL)writeCSVToURL:(NSURL *)absoluteURL error:(NSError **)outError{
    NSDictionary* errDict;
    int result = [dataSource writeDelectusCSV:absoluteURL];
    if(result == ERR_NO_ERROR){
        return YES;
    }else{
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:@"Save Failed", NSLocalizedDescriptionKey,[absoluteURL path], NSFilePathErrorKey, nil];
        *outError = [[NSError errorWithDomain:@"DelectusSaveError" code:result userInfo:errDict] autorelease];
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
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:@"Save Failed", NSLocalizedDescriptionKey, nil];
        *outError = [[NSError errorWithDomain:@"DelectusSaveError" code:3 userInfo:errDict] autorelease];
        return NO;
    }
    return NO; // default--should never be reached
}


@end
