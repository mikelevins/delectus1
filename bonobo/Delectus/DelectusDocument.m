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
    int colcount = [tableView numberOfColumns];
    if(colcount>0){
        NSArray* cols = [tableView tableColumns];
        for(int i=(colcount-1);i>=0;i--){
            NSTableColumn* col = [cols objectAtIndex:i];
            [tableView removeTableColumn:col];
            [col release];
        }
    }
    NSArray* columnLabels = [dataSource collectColumns];
    BOOL includeDeleted = [dataSource includeDeleted];
    NSFont* headerFont = [NSFont systemFontOfSize:12.0];
    int labelcount = [columnLabels count];
    for(int i = 0;i<labelcount;i++){
        NSString* label = (NSString*)[columnLabels objectAtIndex:i];
        NSTableColumn* col = [[NSTableColumn alloc] initWithIdentifier: label];
        BOOL isColDeleted = [dataSource isColumnDeleted:label];
        [col retain];
        [[col headerCell] setStringValue: label];
        [[col headerCell] setFont:headerFont];
        [[col dataCell] setFont:contentFont];
        [tableView addTableColumn: col];
        if(includeDeleted){
            [col setHidden:NO];
        }else{
            if(isColDeleted){
                [col setHidden:YES];
            }else{
                [col setHidden:NO];
            }
        }
    }
    [tableView setAutosaveName:@"delectus1.0"];
    [tableView setAutosaveTableColumns:YES];
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    [super windowControllerDidLoadNib:aController];
    if (dataSource==nil){
        dataSource=[[[NSApp delegate] newDelectus] retain];
    }
    [tableView setDataSource: dataSource];
    [tableView setDelegate: self];
    [tableView setTarget:self];
    [tableView setAction:@selector(clickColumn:)];    
    [addColumnButton setTarget:self];
    [delColumnButton setTarget:self];
    [addRowButton setTarget: self];
    [delRowButton setTarget: self];
    [showDeletedButton setTarget: self];
    [self setupColumns];
    [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
    [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
    [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
}

// --------------------------------------------------------------------------------
// IBActions
// --------------------------------------------------------------------------------

- (IBAction)addRow:(id)sender{
    int err = [dataSource addRow];
    if (err == ERR_NO_ERROR){
        [tableView reloadData];
        int rowCount = [tableView numberOfRows];
        if(rowCount>0){
            [tableView selectRowIndexes:[NSIndexSet indexSetWithIndex: (rowCount-1)] byExtendingSelection:NO];
            [tableView scrollRowToVisible:(rowCount-1)];
        }
        [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
        [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
        [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
    }else{
        NSString* msg = [NSString stringWithFormat: @"There was an error adding a row"];
        NSRunAlertPanel(@"Adding a Row",msg,@"Okay", nil, nil);
    }
}

- (IBAction)toggleRowDeleted:(id)sender{
    int rowIndex=[tableView selectedRow];
    if(rowIndex>=0){
        // first make the window take over FirstResponder status, to force 
        // any active cells to end editing
        [documentWindow makeFirstResponder: nil];
        // Then mark the row
        [dataSource markRow:rowIndex deleted: ![dataSource isRowDeleted: rowIndex]];
        [self updateChangeCount: NSChangeDone];
        [tableView reloadData];
        [tableView deselectAll: self];
    }
}

- (IBAction)addColumn:(id)sender{
    [NSApp beginSheet: addColumnSheet 
       modalForWindow: documentWindow 
        modalDelegate: self 
       didEndSelector: @selector(sheetDidEnd:returnCode:contextInfo:) 
          contextInfo: @"AddColumn"];
}

- (IBAction)toggleColumnDeleted:(id)sender{
    int colIndex=[tableView selectedColumn];
    if(colIndex>=0){
        BOOL includeDeleted = [dataSource includeDeleted];
        NSTableColumn* col = (NSTableColumn*)[[tableView tableColumns] objectAtIndex: colIndex];
        NSString* label = (NSString*)[col identifier];
        // first make the window take over FirstResponder status, to force 
        // any active cells to end editing
        [documentWindow makeFirstResponder: nil];
        // Then mark the column
        [dataSource markColumn:label deleted: ![dataSource isColumnDeleted: label]];
        BOOL isColDeleted = [dataSource isColumnDeleted:label];
        if(includeDeleted){
            [col setHidden:NO];
        }else{
            if(isColDeleted){
                [col setHidden:YES];
            }else{
                [col setHidden:NO];
            }
        }
        [self updateChangeCount: NSChangeDone];
        [tableView reloadData];
        [tableView deselectAll: self];
    }
}

- (IBAction)toggleShowDeleted:(id)sender{
    if([sender state]==NSOnState){
        NSString* sortColumn = [dataSource sortColumn];
        int sortOrder = [dataSource sortOrder];
        NSString* filterText = [filterField stringValue];
        [dataSource getViewIncludingDeleted:YES withSortColumn:sortColumn andSortOrder:sortOrder andFilterText:filterText];
        [tableView reloadData];
        NSArray* cols = [tableView tableColumns];
        int colCount = [cols count];
        for(int i=0;i<colCount;i++){
            NSTableColumn* col=[cols objectAtIndex:i];
            [col setHidden:NO];
        }
        [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
        [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
        [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
        [deletedColsField setHidden:NO];
        [deletedRowsField setHidden:NO];
    }else{
        NSString* sortColumn = [dataSource sortColumn];
        int sortOrder = [dataSource sortOrder];
        NSString* filterText = [filterField stringValue];
        [dataSource getViewIncludingDeleted:NO withSortColumn:sortColumn andSortOrder:sortOrder andFilterText:filterText];
        [tableView reloadData];
        NSArray* cols = [tableView tableColumns];
        int colCount = [cols count];
        for(int i=0;i<colCount;i++){
            NSTableColumn* col=[cols objectAtIndex:i];
            NSString* label = [col identifier];
            BOOL isDeleted = [dataSource isColumnDeleted:label];
            if(isDeleted){                
                [col setHidden:YES];
            }else{
                [col setHidden:NO];
            }
        }
        [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
        [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
        [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
        [deletedColsField setHidden:YES];
        [deletedRowsField setHidden:YES];
    }
}

- (IBAction)emptyTrash:(id)sender{
    NSRunAlertPanel(@"Purge",@"Purging deleted items",@"Okay", nil, nil);
}

- (IBAction)renameColumn:(id)sender{
    NSRunAlertPanel(@"Rename",@"Renaming a column",@"Okay", nil, nil);
}

- (IBAction)setFilter:(id)sender{
    NSString* sortColumn = [dataSource sortColumn];
    int sortOrder = [dataSource sortOrder];
    BOOL includeDeleted = [dataSource includeDeleted];
    NSString* filterText = [filterField stringValue];
    [dataSource getViewIncludingDeleted:includeDeleted withSortColumn:sortColumn andSortOrder:sortOrder andFilterText:filterText];
    [tableView reloadData];
    [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
    [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
    [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
}

-(void)advanceSortForColumn:(NSTableColumn*)aColumn{
    NSString* sortColumn = [dataSource sortColumn];
    int sortOrder = [dataSource sortOrder];
    BOOL includeDeleted = [dataSource includeDeleted];
    NSString* nextLabel = [aColumn identifier];
    if([nextLabel isEqualTo: sortColumn]){
        if (sortOrder == SORT_ASCENDING){
            sortOrder=SORT_DESCENDING;
            [tableView setIndicatorImage:[NSImage imageNamed: @"NSDescendingSortIndicator"] inTableColumn:aColumn];
        } else if (sortOrder == SORT_DESCENDING){
            sortOrder=SORT_NONE;
            [tableView setIndicatorImage:nil inTableColumn:aColumn];
        } else {
            sortOrder=SORT_ASCENDING;
            [tableView setIndicatorImage:[NSImage imageNamed: @"NSAscendingSortIndicator"] inTableColumn:aColumn];
        }
    }else{
        NSTableColumn* lastCol = [tableView tableColumnWithIdentifier: sortColumn];
        sortOrder=SORT_ASCENDING;
        [tableView setIndicatorImage:nil inTableColumn:lastCol];
        [tableView setIndicatorImage:[NSImage imageNamed: @"NSAscendingSortIndicator"] inTableColumn:aColumn];
    }
    [dataSource getViewIncludingDeleted:includeDeleted withSortColumn:nextLabel andSortOrder:sortOrder andFilterText:[filterField stringValue]];
    [tableView reloadData];
    [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
    [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
    [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
}

- (void)clickColumn:(id)sender{
    BOOL inRow = [sender clickedRow] > -1;
    BOOL inCol = [sender clickedColumn] > -1;
    if(inRow){
        [sender editColumn:[sender clickedColumn] row:[sender clickedRow] withEvent:nil  select:YES];
    }else if(inCol){
        NSInteger col_index = [sender clickedColumn];
        NSArray* cols = [sender tableColumns];
        NSTableColumn* col = (NSTableColumn*)[cols objectAtIndex: col_index];
        [self advanceSortForColumn:col];
    }
}

- (IBAction)acceptNewColumn:(id)sender{
    [NSApp endSheet:addColumnSheet];
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

// ----------------------------------------
// NSTableView delegate methods
// ----------------------------------------

- (void)tableView:(NSTableView *)aTableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex{
    NSString* label = [aTableColumn identifier];
    BOOL isColDeleted = [dataSource isColumnDeleted:label];
    BOOL isRowDeleted = [dataSource isRowDeleted:rowIndex];
    BOOL isCellDeleted = (isColDeleted||isRowDeleted);
    BOOL isRowEven = ((rowIndex%2)==0);
    if(isCellDeleted){
        NSColor* cellColor;
        if(isRowEven){
            cellColor=[NSColor colorWithCalibratedHue:0.0 saturation:0.25 brightness:1.0 alpha:1.0];
        }else{
            cellColor=[NSColor colorWithCalibratedHue:0.0 saturation:0.50 brightness:1.0 alpha:1.0];
        }
        [aCell setBackgroundColor: cellColor];
        [aCell setDrawsBackground: YES];
        [aCell setEditable:NO];
        [aCell setSelectable:NO];
    } else {
        [aCell setDrawsBackground: NO];
        [aCell setEditable:YES];
        [aCell setSelectable:YES];
    }
}


// handle sheets

- (void)sheetDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo{
    [sheet orderOut:self];
    
    NSString* commandStr = (NSString*)contextInfo;
    if([commandStr isEqualTo: @"AddColumn"]){
        NSString* lbl = [addColumnLabelField stringValue];
        BOOL isDup = [dataSource isDuplicateLabel: lbl];
        if (isDup){
            NSString* msg = [NSString stringWithFormat: @"The label '%@' is already in use",lbl];
            NSRunAlertPanel(@"Adding a Column",msg,@"Okay", nil, nil);
        } else {
            int err = [dataSource addColumn:lbl];
            if (err == ERR_NO_ERROR){
                [self setupColumns];
                [tableView reloadData];
                int colCount = [tableView numberOfColumns];
                if(colCount>0){
                    [tableView selectColumnIndexes:[NSIndexSet indexSetWithIndex: (colCount-1)] byExtendingSelection:NO];
                    [tableView scrollColumnToVisible:(colCount-1)];
                }
                [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
                [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
                [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
            }else{
                NSString* msg = [NSString stringWithFormat: @"There was an error adding the column named '%@'",lbl];
                NSRunAlertPanel(@"Adding a Column",msg,@"Okay", nil, nil);
            }
        }
    }
}

@end
