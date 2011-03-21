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
#import "DelectusTotalsDataSource.h"
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

- (void)setupColumns{
    // tableview setup
    NSSize spacing;
    spacing.width=1.0;
    spacing.height=0.0;
    [tableView setIntercellSpacing:spacing];
    [totalsView setIntercellSpacing:spacing];
    
    // index column
    NSTableColumn* indexCol = [[NSTableColumn alloc] initWithIdentifier: @"#"];
    NSTextFieldCell* indexCell = [[[NSTextFieldCell alloc] init] retain];
    [indexCell setDrawsBackground: YES];
    [indexCell setBackgroundColor: [NSColor lightGrayColor]];
    [indexCell setTextColor: [NSColor whiteColor]];
    [indexCell setAlignment: NSRightTextAlignment];
    [indexCol setDataCell: indexCell];
    [indexCol setEditable:NO];
    [indexCol setMinWidth:24];
    [indexCol setMaxWidth:48];
    [[indexCol headerCell] setStringValue: @"#"];
    [tableView addTableColumn: indexCol];
    
    // to-do column
    NSTableColumn* todoCol = [[[NSTableColumn alloc] initWithIdentifier: @"?"] retain];
    NSButtonCell* todoCell = [[[NSButtonCell alloc] init] retain];
    [todoCell setButtonType:NSSwitchButton];
    [todoCell setTitle:@""];
    [todoCell setBackgroundStyle: NSBackgroundStyleDark];
    [todoCol setDataCell: todoCell];
    [todoCol setMinWidth:24];
    [todoCol setMaxWidth:24];
    [[todoCol headerCell] setStringValue: @"?"];
    [tableView addTableColumn: todoCol];
    [todoCol setHidden:YES];
    
    // columns
    NSArray* columnLabels = [dataSource collectColumns];
    int colcount = [columnLabels count];
    for(int i = 0;i<colcount;i++){
        NSString* label = (NSString*)[columnLabels objectAtIndex:i];
        NSTableColumn* col = [[NSTableColumn alloc] initWithIdentifier: label];
        [[col headerCell] setStringValue: label];
        [tableView addTableColumn: col];
    }
    
    [tableView setRowHeight:(24)];
    
    // totals view
    NSTableColumn* dummyIndexCol = [[NSTableColumn alloc] initWithIdentifier: @"#"];
    NSTableColumn* dummyToDoCol = [[NSTableColumn alloc] initWithIdentifier: @"?"];
    [dummyToDoCol setHidden:YES];
    [totalsView addTableColumn: dummyIndexCol];
    [totalsView addTableColumn: dummyToDoCol];
    for(int i = 0;i<colcount;i++){
        NSString* label = (NSString*)[columnLabels objectAtIndex:i];
        NSTableColumn* col = [[NSTableColumn alloc] initWithIdentifier: label];
        [[col headerCell] setStringValue: label];
        [totalsView addTableColumn: col];
    }
    NSArray* tableCols=[tableView tableColumns];
    NSArray* totalCols=[totalsView tableColumns];
    int colCount = [tableCols count];
    for(int j=0;j<colCount;j++){
        NSTableColumn* tableCol = (NSTableColumn*)[tableCols objectAtIndex:j];
        NSTableColumn* totalCol = (NSTableColumn*)[totalCols objectAtIndex:j];
        [totalCol setWidth:[tableCol width]];
    }
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    [super windowControllerDidLoadNib:aController];
    if (dataSource==nil){
        dataSource=[[[NSApp delegate] newDelectus] retain];
    }
    totalsDataSource=[[DelectusTotalsDataSource alloc] initWithDataSource: dataSource];
    [self setupColumns];
    [tableView setDataSource: dataSource];
    [totalsView setDataSource: totalsDataSource];
}

// --------------------------------------------------------------------------------
// IBActions
// --------------------------------------------------------------------------------

- (IBAction)toggleToDo:(id)sender{
    NSInteger state = [sender state];
    NSTableColumn* todoCol1 = [tableView tableColumnWithIdentifier:@"?"];
    NSTableColumn* todoCol2 = [totalsView tableColumnWithIdentifier:@"?"];
    if (state == NSOnState){
        [todoCol1 setHidden:NO];
        [todoCol2 setHidden:NO];
    }else{
        [todoCol1 setHidden:YES];
        [todoCol2 setHidden:YES];
    }
}

- (IBAction)addRow:(id)sender{}

- (IBAction)toggleRowDeleted:(id)sender{}

- (IBAction)addColumn:(id)sender{}

- (IBAction)toggleColumnDeleted:(id)sender{}

- (IBAction)toggleTotals:(id)sender{
    NSInteger state = [sender state];
    NSRect viewFrame;
    if (state == NSOnState){
        [totalsScrollView setHidden: NO];
        [totalsScrollView setNeedsDisplay:YES];
        viewFrame = [tableScrollView frame];
        viewFrame.size.height-=24;
        viewFrame.origin.y+=24;
        [tableScrollView setFrame:viewFrame];
        [tableScrollView setNeedsDisplay:YES];
    }else{
        [totalsScrollView setHidden: YES];
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
            *outError = [NSError errorWithDomain:errStr code:2 userInfo:errDict];
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
            *outError = [NSError errorWithDomain:errStr code:2 userInfo:errDict];
            return NO;
        }else{
            dataSource=[src retain];
            return YES;
        }
    } else {
        errStr=@"FileFormatError";
        errMsg=@"Unrecognized file type";
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:errMsg, NSLocalizedDescriptionKey,[absoluteURL path], NSFilePathErrorKey, nil];
        *outError = [NSError errorWithDomain:errStr code:2 userInfo:errDict];
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
        *outError = [NSError errorWithDomain:@"DelectusSaveError" code:result userInfo:errDict];
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
        *outError = [NSError errorWithDomain:@"DelectusSaveError" code:result userInfo:errDict];
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
        *outError = [NSError errorWithDomain:@"DelectusSaveError" code:3 userInfo:errDict];
        return NO;
    }
    return NO; // default--should never be reached
}


@end
