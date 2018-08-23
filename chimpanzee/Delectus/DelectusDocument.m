//
//  DelectusDocument.m
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 mikel evins. All rights reserved.
//

#import "DelectusDocument.h"
#import "DelectusDelegate.h"
#import "DelectusDataSource.h"
#import "DelectusDataCell.h"
#import "DelectusPrintView.h"
#define ___VERSION 406001
#include "gambit.h"
#include "Delectus.h"

@implementation DelectusDocument

#pragma mark - init
// --------------------------------------------------------------------------------
// init
// --------------------------------------------------------------------------------

- (id)init
{
    self = [super init];
    if (self) {
        [[NSUserDefaults standardUserDefaults] synchronize];
        columnInfo = [[NSMutableDictionary dictionaryWithCapacity:8] retain];
    }
    return self;
}

#pragma mark - accessors
// --------------------------------------------------------------------------------
// Accessors
// --------------------------------------------------------------------------------

- (NSTableView*)tableView{
    return tableView;
}

- (DelectusDataSource*)dataSource{
    return dataSource;
}

- (BOOL)deletedItemsAreShown{
    return [showDeletedButton state];
}

- (BOOL)isPristine{
    BOOL result=YES;
    if([self isDocumentEdited]){result=NO;}
    if([tableView numberOfColumns]>0){result=NO;};
    if([tableView numberOfRows]>0){result=NO;};
    return result;
}


#pragma mark - window init
// --------------------------------------------------------------------------------
// Window init
// --------------------------------------------------------------------------------

- (NSString *)windowNibName
{
    return @"DelectusDocument";
}

- (void)recordColumnInfo{
    int colcount = [tableView numberOfColumns];
    NSMutableDictionary* newWidthInfo=[NSMutableDictionary dictionaryWithCapacity:colcount];
    NSMutableArray* newOrderInfo=[NSMutableArray array];
    if(colcount>0){
        NSArray* cols = [tableView tableColumns];
        for(int i=0;i<colcount;i++){
            NSTableColumn* col = [cols objectAtIndex:i];
            NSString* label = [col identifier];
            NSNumber* colWidth = [NSNumber numberWithFloat:[col width]];
            [newWidthInfo setValue:colWidth forKey: label];
            [newOrderInfo addObject:label];
        }
    }
    
    if(columnInfo==NULL){
        columnInfo = [[NSMutableDictionary dictionaryWithCapacity:2] retain]; 
    }
    
    [columnInfo setObject:newWidthInfo forKey:@"widthInfo"];
    [columnInfo setObject:newOrderInfo forKey:@"orderInfo"];
    
    NSURL* docURL = [self fileURL];
    if (docURL!=NULL){
        NSString* docPath = [docURL path];
        NSDictionary* oldColumnInfoDict = (NSDictionary*)[[NSUserDefaults standardUserDefaults] objectForKey:@"DelectusColumnInfoDict"];
        NSMutableDictionary* newColumnInfoDict = [NSMutableDictionary dictionaryWithCapacity:(colcount+[oldColumnInfoDict count])];
        [newColumnInfoDict addEntriesFromDictionary:oldColumnInfoDict];
        [newColumnInfoDict setValue:columnInfo forKey: docPath];
        [[NSUserDefaults standardUserDefaults] setObject:newColumnInfoDict forKey:@"DelectusColumnInfoDict"];
    }
}

- (void) closePristineDocuments:sender {
    int i;
    NSArray* docs = [[NSDocumentController sharedDocumentController] documents];
    for(i=0;i<[docs count];i++){
        DelectusDocument* doc = (DelectusDocument*)[docs objectAtIndex:i];
        if ([doc isPristine]&&(doc!=sender)){[doc close];}
    }
}

- (void)setupSpacing{
    // tableview setup
    NSSize spacing;
    spacing.width=1.0;
    spacing.height=0.0;
    [tableView setIntercellSpacing:spacing];
}

- (void)setupColumns{
    int colcount = [tableView numberOfColumns];
    if(colcount>0){
        NSArray* cols = [NSArray arrayWithArray:[tableView tableColumns]];
        for(int i=0;i<colcount;i++){
            NSTableColumn* col = [cols objectAtIndex:i];
            [tableView removeTableColumn:col];
            [col release];
        }
    }
    [self setupSpacing];
    NSDictionary* columnInfoDict = (NSDictionary*)[[NSUserDefaults standardUserDefaults] objectForKey:@"DelectusColumnInfoDict"];
    if(columnInfoDict!=NULL){
        NSURL* docURL = [self fileURL];
        NSString* docStr = [docURL path];
        NSDictionary* savedColumnInfo = [columnInfoDict objectForKey:docStr];
        if(savedColumnInfo!=NULL){
            [columnInfo addEntriesFromDictionary:savedColumnInfo];
        }
    }
    
    NSArray* sourceLabels = [dataSource collectColumns];
    NSArray* savedLabels = [columnInfo objectForKey:@"orderInfo"];
    NSDictionary* savedWidths = [columnInfo objectForKey:@"widthInfo"];
    
    NSMutableArray* columnLabels = [[NSMutableArray arrayWithArray:savedLabels] retain];
    //remove labels that are in the saved set but not in the source set
    for(int i=([columnLabels count]-1);i>=0;i--){
        NSString* lbl = [columnLabels objectAtIndex:i];
        NSInteger foundIndex = [sourceLabels indexOfObject:lbl];
        if(foundIndex==NSNotFound){[columnLabels removeObjectAtIndex:i];}
    }
    // add labels that are in the source set but not in the saved set
    for(int j=0;j<[sourceLabels count];j++){
        NSString* lbl = [sourceLabels objectAtIndex:j];
        NSInteger foundIndex = [columnLabels indexOfObject:lbl];
        if(foundIndex==NSNotFound){[columnLabels addObject:lbl];}
    }
        
    NSFont* headerFont = [NSFont systemFontOfSize:13.0];
    NSFont* contentFont = [[NSApp delegate] contentFont];
    int labelcount = [columnLabels count];
    for(int i = 0;i<labelcount;i++){
        NSString* label = (NSString*)[columnLabels objectAtIndex:i];
        NSTableColumn* col = [[NSTableColumn alloc] initWithIdentifier: label];
        NSNumber* colWidth;
        if(savedWidths==NULL){
            colWidth=[NSNumber numberWithFloat:190.0];            
        }else{
            colWidth=[savedWidths objectForKey:label];
            if(colWidth==NULL){
                colWidth=[NSNumber numberWithFloat:190.0];            
            }
        }
        [col retain];
        [[col headerCell] setStringValue: label];
        [[col headerCell] setFont:headerFont];
        [col setDataCell:[[DelectusDataCell alloc] init]];
        [[col dataCell] setFont:contentFont];
        [[col dataCell] setLineBreakMode:NSLineBreakByCharWrapping];
        [[col dataCell] setTruncatesLastVisibleLine:YES];
        [col setMinWidth:64.0];
        [col setMaxWidth:600.0];
        if(colWidth==NULL){
            [col setWidth:196.0];
        }else{
            [col setWidth:[colWidth floatValue]];
        }
        [tableView addTableColumn: col];
    }
    [tableView setRowHeight:(1.8*[contentFont pointSize])];
}

- (void)updateUIForSelectionChange{
    BOOL isAColumnSelected = ([tableView selectedColumn] != -1);
    BOOL isARowSelected = ([tableView selectedRow] != -1);
    if(isAColumnSelected){
        [delColumnButton setEnabled:YES];
    }else{
        [delColumnButton setEnabled:NO];
    }
    if(isARowSelected){
        [delRowButton setEnabled: YES];
    }else{
        [delRowButton setEnabled: NO];
    }
    [[NSApp delegate] updateUIForDocument:self withSelectedColumn: [tableView selectedColumn] andRow:[tableView selectedRow]];
}


- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    [self closePristineDocuments:self];
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
    [self updateUIForSelectionChange];
    [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
    [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
    [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
    // make sure this doc ends up in front
    NSArray* controllers = [self windowControllers];
    if([controllers count]>0){
        NSWindowController* controller = (NSWindowController*)[controllers objectAtIndex:0];
        NSWindow* win = [controller window];
        [win makeKeyAndOrderFront: self];
    }    
}

- (void)printShowingPrintPanel:(BOOL)showPanels {
    NSSize paperSize = [[self printInfo] paperSize];
    NSRect boundsRect = NSMakeRect(0,0,(3*paperSize.width/4),1);
    NSView *printView = [[DelectusPrintView alloc] initWithFrame:boundsRect withDataSource:dataSource andDocumentName:[self displayName]];
    // enable pagination
    [[self printInfo] setHorizontalPagination: NSAutoPagination];
    [[self printInfo] setVerticalPagination: NSAutoPagination];
    // Construct the print operation and setup Print panel
    NSPrintOperation *op = [NSPrintOperation
                            printOperationWithView:printView
                            printInfo:[self printInfo]];
    //    [op setShowPanels:showPanels];
    [op setShowsPrintPanel:YES];
    
    // Run operation, which shows the Print panel if showPanels was YES
    [self runModalPrintOperation:op
                        delegate:nil
                  didRunSelector:NULL
                     contextInfo:NULL];
}

#pragma mark - IBActions
// --------------------------------------------------------------------------------
// IBActions
// --------------------------------------------------------------------------------

- (IBAction)addRow:(id)sender{
    NSInteger colCount = [dataSource countColumns];
    if(colCount>0){
        int err = [dataSource addRow];
        if (err == ERR_NO_ERROR){
            [self updateChangeCount: NSChangeDone];
            [tableView reloadData];
            int rowCount = [tableView numberOfRows];
            if(rowCount>0){
                [tableView scrollRowToVisible:(rowCount-1)];
                [tableView selectRowIndexes:[NSIndexSet indexSetWithIndex: (rowCount-1)] byExtendingSelection:NO];
                [tableView editColumn:0 row:(rowCount-1) withEvent:nil select:YES];
            }
            [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
            [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
            [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
        }else{
            NSString* msg = [NSString stringWithFormat: @"There was an error adding a row"];
            NSRunAlertPanel(@"Adding a Row",msg,@"Okay", nil, nil);
        }
    }else{
        NSString* msg = [NSString stringWithFormat: @"Can't add a row with no columns!"];
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
        if([dataSource isRowDeleted: rowIndex]){
            [dataSource markRow:rowIndex deleted:NO];
        }else{
            [dataSource markRow:rowIndex deleted:YES];
        }
        [self updateChangeCount: NSChangeDone];
        [tableView reloadData];
        [tableView deselectAll: self];
        [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
        [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
        [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
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
        [documentWindow makeFirstResponder: nil];
        NSTableColumn* col = (NSTableColumn*)[[tableView tableColumns] objectAtIndex: colIndex];
        NSString* label = (NSString*)[col identifier];
        // Then mark the column
        BOOL isColDeleted = [dataSource isColumnDeleted: label];
        if(isColDeleted){
            [dataSource markColumn:label deleted:NO];
        }else{
            [dataSource markColumn:label deleted:YES];
        }
        [self updateChangeCount: NSChangeDone];
        [self setupColumns];
        [tableView reloadData];
        [tableView deselectAll: self];
        [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
        [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
    }
}

- (IBAction)toggleShowDeleted:(id)sender{
    NSString* sortColumn = [dataSource sortColumn];
    int sortOrder = [dataSource sortOrder];
    NSString* filterText = [filterField stringValue];
    [tableView deselectAll: self];
    if([showDeletedButton state]==NSOnState){
        [dataSource getViewIncludingDeleted:YES withSortColumn:sortColumn andSortOrder:sortOrder andFilterText:filterText];
        [deletedColsField setHidden:NO];
        [deletedRowsField setHidden:NO];
    }else{
        [dataSource getViewIncludingDeleted:NO withSortColumn:sortColumn andSortOrder:sortOrder andFilterText:filterText];
        [deletedColsField setHidden:YES];
        [deletedRowsField setHidden:YES];
    }
    [self setupColumns];
    [tableView reloadData];
    [self updateUIForSelectionChange];
    [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
    [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
    [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
}

- (IBAction)performShowDeletedClick:(id)sender{
    [[showDeletedButton cell] performClick:sender];
}

- (IBAction)emptyTrash:(id)sender{
    int deletedRows = [dataSource countDeletedRows];
    int deletedColumns = [dataSource countDeletedColumns];
    if((deletedRows>0)||(deletedColumns>0)){
        [documentWindow makeFirstResponder: nil]; // ends cell editing
        [dataSource compact];
        [self updateChangeCount: NSChangeDone];
        [self setupColumns];
        [tableView reloadData];
        [self updateUIForSelectionChange];
        [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
        [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
        [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
    }
}

- (IBAction)renameColumn:(id)sender{
    [NSApp beginSheet: addColumnSheet 
       modalForWindow: documentWindow 
        modalDelegate: self 
       didEndSelector: @selector(sheetDidEnd:returnCode:contextInfo:) 
          contextInfo: @"RenameColumn"];
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

#pragma mark - Clicking columns
// --------------------------------------------------------------------------------
// Clicking columns
// --------------------------------------------------------------------------------

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
            nextLabel=nil;
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

#pragma mark - Handling font changes
// --------------------------------------------------------------------------------
// Handling font changes
// --------------------------------------------------------------------------------

- (void)setFont:(NSFont*)newFont{
    [[NSApp delegate] setContentFont: newFont];
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
    NSFont *oldFont = [[NSApp delegate] contentFont];
    NSFont *newFont = [sender convertFont:oldFont];
    [self setFont:newFont];
    return;
}

#pragma mark - NSDocument APIs
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
            [self updateChangeCount: NSChangeDone];
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
            [self updateChangeCount: NSChangeDone];
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

#pragma mark - NSTableView delegate methods
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
        [[aTableColumn dataCell] setIsRenderingDeleted:YES];
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
        [[aTableColumn dataCell] setIsRenderingDeleted:NO];
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
                [self updateChangeCount: NSChangeDone];
                [self setupColumns];
                [tableView reloadData];
                [self recordColumnInfo];
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
    } else if([commandStr isEqualTo: @"RenameColumn"]){
        NSInteger colIndex = [tableView selectedColumn];
        if (colIndex>(-1)){
            NSTableColumn* col = [[tableView tableColumns] objectAtIndex:colIndex];
            NSString* oldlbl = [col identifier];
            NSString* newlbl = [addColumnLabelField stringValue];
            BOOL isDup = [dataSource isDuplicateLabel: newlbl];
            if(isDup && (![newlbl isEqualTo: oldlbl])){
                NSString* msg = [NSString stringWithFormat: @"The label '%@' is already in use",newlbl];
                NSRunAlertPanel(@"Renaming a Column",msg,@"Okay", nil, nil);
            }else{
                int err = [dataSource renameColumn:oldlbl to:newlbl];
                if (err == ERR_NO_ERROR){
                    [self updateChangeCount: NSChangeDone];
                    [self setupColumns];
                    [tableView reloadData];
                    [self recordColumnInfo];
                    [itemCountField setStringValue:[NSString stringWithFormat:@"%d items",[tableView numberOfRows]]];
                    [deletedColsField setStringValue:[NSString stringWithFormat:@"%d columns",[dataSource countDeletedColumns]]];
                    [deletedRowsField setStringValue:[NSString stringWithFormat:@"%d rows",[dataSource countDeletedRows]]];
                }else{
                    NSString* msg = [NSString stringWithFormat: @"There was an error changing the column name to '%@'",newlbl];
                    NSRunAlertPanel(@"Renaming a Column",msg,@"Okay", nil, nil);
                }
            }
        }else{
            NSString* msg = [NSString stringWithFormat: @"Error: no column selected"];
            NSRunAlertPanel(@"Renaming a Column",msg,@"Okay", nil, nil);
        }
    }
}

// track selection changes

- (void)tableViewSelectionDidChange:(NSNotification *)aNotification{
    [self updateUIForSelectionChange];
}

- (void)tableViewColumnDidResize:(NSNotification *)aNotification{
    [self recordColumnInfo];
}


- (void)tableViewColumnDidMove:(NSNotification *)aNotification{
    [self recordColumnInfo];
}

@end
