// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          DelectusDocument.m
// Project:       Delectus
// Purpose:       Document implementation
// Author:        mikel evins
//
// ***********************************************************************

#import "DelectusDocument.h"
#import "DelectusAppDelegate.h"
#import "NSString+CString.h"
#import "DelectusCell.h"

@implementation DelectusDocument

// ========================================
// private
// ========================================

- (BOOL)isEmptyString:(NSString*)aString{
    if (aString==NULL){return YES;}
    if ([aString isEqualToString: @""]){return YES;}
    return NO;
}

- (BOOL)isDuplicateLabel:(NSString*)aLabel{
    return is_duplicate_label(documentID,[aLabel asCString]);
}

- (int)indexOfColumnLabel: (NSString*)aLabel{
    int i;
    NSTableColumn* col;
    NSString* lbl;
    int result = NSNotFound;
    NSArray* cols = [tableView tableColumns];
    for(i=0;i<[cols count];i++){
        col = (NSTableColumn*)[cols objectAtIndex:i];
        lbl = (NSString*)[col identifier];
        if([aLabel isEqualToString: lbl]){ result=i; return result;}
    }
    return result;
}

- (void)setupColumns{
    int i;
    NSArray* doc_column_labels = (NSArray*)get_column_labels(documentID);

    // 1. clear the columns
    NSArray* view_columns = [tableView tableColumns];
    NSMutableArray* cols_to_delete = [NSMutableArray arrayWithCapacity:[view_columns count]];
    for(i=0;i<[view_columns count];i++){
        NSTableColumn* col = (NSTableColumn*)[view_columns objectAtIndex:i];
        NSString* lbl = (NSString*)[col identifier];
        int foundIndex = [doc_column_labels indexOfObject: lbl];
        [cols_to_delete addObject: col];
    }
    for(i=0;i<[cols_to_delete count];i++){
        NSTableColumn* col = (NSTableColumn*)[cols_to_delete objectAtIndex:i];
        [tableView removeTableColumn: col];
    }

    // 2. add columns in the order specified by the retrieved labels
    for(i=0;i<[doc_column_labels count];i++){
        NSString* lbl = (NSString*)[doc_column_labels objectAtIndex:i];
        NSTableColumn* new_col;
        NSCell* hcell;
        DelectusCell* vcell = [[DelectusCell alloc] init];;
        new_col = [[NSTableColumn alloc] initWithIdentifier: lbl];
        hcell = [new_col headerCell];
        [hcell setStringValue: lbl];
        [new_col setDataCell: vcell];
        [tableView addTableColumn: new_col];
    }

    // 3. iterate over all columns; for each column, set its
    //    width. Then, if it is deleted, and show-deleted? is false,
    //    hide the column. Otherwise, show it
    NSArray* new_columns = [tableView tableColumns];
    BOOL are_deleteds_shown = are_deleted_items_shown(documentID);
    for(i=0;i<[new_columns count];i++){
        NSTableColumn* col = (NSTableColumn*)[new_columns objectAtIndex:i];
        NSString* lbl = (NSString*)[col identifier];
        BOOL is_deleted = is_column_deleted(documentID,[lbl asCString]);
        int col_width = get_column_width(documentID,[lbl asCString]);
        [col setMinWidth: 50.0];
        [col setWidth: (CGFloat)col_width];
        if(is_deleted && !are_deleteds_shown){ 
            [col setHidden: YES]; 
        } else {
            [col setHidden: NO]; 
        }
    }
}


// ========================================
// PUBLIC
// ========================================
// ----------------------------------------
// initialization
// ----------------------------------------

- (id)init {
    self = [super init];
    if (self) {
        documentID = NO_DOCUMENT;
        isPristine=YES;
    }
    return self;
}

// ----------------------------------------
// accessors
// ----------------------------------------

- (void)reloadData{
    [self setupColumns];
    [tableView reloadData];
    [self updateTrash];
    [self updateItemCount];
}

- (int)documentID{ return documentID; }

- (BOOL)isPristine {return isPristine;}

- (BOOL)isColumnSelected:(NSTableColumn *)aTableColumn {
    NSArray* cols = [tableView tableColumns];
    int selectedColIndex = [tableView selectedColumn];
    NSTableColumn* selectedColumn=nil;
    BOOL isCellSelected=NO;
    if(selectedColIndex>=0){selectedColumn = (NSTableColumn*)[cols objectAtIndex: selectedColIndex];}
    if(aTableColumn==selectedColumn){return YES;}else{return NO;}
}

- (BOOL)isRowSelected:(NSInteger)rowIndex {
    int selectedRowIndex = [tableView selectedRow];
    if(rowIndex==selectedRowIndex){return YES;}else{return NO;}
}

// ----------------------------------------
// NSDocument APIs
// ----------------------------------------

// returns a document ID
- (int)readCSVFile:(NSURL *)absoluteURL withHeadersInFirstLine: (BOOL)headersInFirstLine {
    NSString* fPath = [absoluteURL path];
    return read_csv_from_path([fPath asCString],headersInFirstLine);
}

// returns a document ID
- (int)readDelectusFile:(NSURL *)absoluteURL {
    NSString* fPath = [absoluteURL path];
    return read_delectus_from_path([fPath asCString]);
}

- (BOOL)readFromURL: (NSURL *)absoluteURL ofType:(NSString *) typeName error: (NSError **)outError{
    NSString* fPath = [absoluteURL path];
    NSDictionary* errDict;
    NSString *errStr, *errMsg;
    if ([typeName isEqualToString: @"csv"]) {
        errStr=@"CSVFormatError";
        errMsg=@"Couldn't read CSV data from the file";
        documentID = [self readCSVFile:absoluteURL withHeadersInFirstLine:YES];
        isPristine=NO;
    } else if ([typeName isEqualToString: @"delectus"]) {
        errStr=@"DelectusFormatError";
        errMsg=@"Couldn't read Delectus data from the file";
        documentID = [self readDelectusFile:absoluteURL];
        isPristine=NO;
    } else {
        errStr=@"FileFormatError";
        errMsg=@"Unrecognized file type";
        documentID=NO_DOCUMENT;
        isPristine=YES;
    }

    if (documentID != NO_DOCUMENT){
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
    NSString *errStr, *errMsg;
    NSString* savePath;
    if([typeName isEqualToString: @"delectus"]){
        savePath = [absoluteURL path];
        int written_docID = write_delectus_to_path(documentID, [savePath asCString]);
        if(written_docID != NO_DOCUMENT){
            isPristine=NO;
            return YES;
        }else{
            NSString* errMsg = [NSString stringWithFormat: @"Write failed on pathname '%@'",savePath];
            documentID=written_docID;
            NSRunAlertPanel(@"Save Failed",errMsg,nil, nil, nil);   
            errDict = [NSDictionary dictionaryWithObjectsAndKeys:
                                        @"Save Failed", NSLocalizedDescriptionKey,
                                    savePath, NSFilePathErrorKey, nil];
            *outError = [NSError errorWithDomain:@"DelectusSaveError" code:3 userInfo:errDict];
            return NO;
        }
    }else{
        NSString* errMsg = [NSString stringWithFormat: @"Can't save a document of type '%@'",typeName];
        NSRunAlertPanel(@"Save Failed",errMsg,nil, nil, nil);   
        errDict = [NSDictionary dictionaryWithObjectsAndKeys:
                                    @"Save Failed", NSLocalizedDescriptionKey, nil];
        *outError = [NSError errorWithDomain:@"SDelectusSaveError" code:3 userInfo:errDict];
        return NO;
    }
}

- (NSString *)windowNibName{ return @"DelectusDocument";}

- (void) hideTrashLabel{ [toggleShownLabel setStringValue: @""]; }

- (void) showTrashLabel{
    [toggleShownLabel setTextColor: [NSColor redColor]];
    [toggleShownLabel setStringValue: @"Deleted items are shown."];
}

- (void) showEmptyTrash{
    id delegate = [[NSApplication sharedApplication] delegate];
    NSImage* trashImage = [delegate trashEmptyImage];
    if ([trashImage isValid]){
        [toggleShownButton setImage: trashImage];
    } else {
        NSLog(@"Can't set trash image to: %@, it is invalid", trashImage);
    }
}

- (void) showFullTrash{
    id delegate = [[NSApplication sharedApplication] delegate];
    NSImage* trashImage = [delegate trashFullImage];
    if ([trashImage isValid]){
        [toggleShownButton setImage: trashImage];
    } else {
        NSLog(@"Can't set trash image to: %@, it is invalid", trashImage);
    }
}

- (void) updateTrash{
    BOOL deleted_items_shown = are_deleted_items_shown(documentID);
    BOOL any_deleted_items = has_deleted_items(documentID);

    if (deleted_items_shown){
        [self showTrashLabel];
    } else {
        [self hideTrashLabel];
    }

    if (any_deleted_items){
        [self showFullTrash];
    }else{
        [self showEmptyTrash];
    }
}

- (void)updateItemCount{
    int rowCount = number_of_rows(documentID);
    if (rowCount == 1){
        [itemCountField setStringValue: [NSString stringWithFormat: @"1 item"]];
    }else{
        [itemCountField setStringValue: [NSString stringWithFormat: @"%d items", rowCount]];
    }
}

- (void) closePristineDocuments {
    int i;
    NSArray* docs = [[NSDocumentController sharedDocumentController] documents];
    for(i=0;i<[docs count];i++){
        DelectusDocument* doc = (DelectusDocument*)[docs objectAtIndex:i];
        if ([doc isPristine]){[doc close];}
    }
}

- (void) deselectAll{
    [tableView deselectAll: self];
}

- (void)clickColumn:(id)sender{
    if([sender clickedRow] == -1){
        NSInteger col_index = [sender clickedColumn];
        NSArray* cols = [sender tableColumns];
        NSTableColumn* col = (NSTableColumn*)[cols objectAtIndex: col_index];
        NSString* ident = [col identifier];
        [sender selectColumnIndexes:[NSIndexSet indexSetWithIndex: col_index] byExtendingSelection:NO];
        bool is_new_sort = advance_sort(documentID,[ident asCString]);
        if(is_new_sort){
            [sender selectColumnIndexes:[NSIndexSet indexSetWithIndex: col_index] byExtendingSelection:NO];
            [sender scrollColumnToVisible:col_index];
        }else{
            [self deselectAll];
        }
        [self updateChangeCount: NSChangeDone];
    } else{
        [sender editColumn:[sender clickedColumn] row:[sender clickedRow] withEvent:nil  select:YES];
    }
}

- (void)windowControllerDidLoadNib:(NSWindowController *)windowController{
    // if the document ID is invalid, then we're creating a new one; we need to ask the
    // back end for a new empty one
    if(documentID == NO_DOCUMENT){
        documentID=get_new_document();
        isPristine=YES;
    } else {
        // if the document we just opened is not pristine, then we close
        // any pristine (i.e. new, unedited, untitled) documents
        [self closePristineDocuments];
        // make sure this doc ends up in front
        NSArray* controllers = [self windowControllers];
        if([controllers count]>0){
            NSWindowController* controller = (NSWindowController*)[controllers objectAtIndex:0];
            NSWindow* win = [controller window];
            [win makeKeyAndOrderFront: self];
        }
    }

    // check the documentID again to make sure we have a valid doc
    if(documentID != NO_DOCUMENT){
        [self reloadData];
        [tableView setTarget: self];
        [tableView setAction: @selector(clickColumn:)];
        [tableView setAutosaveName: nil];
        [tableView setAutosaveTableColumns: NO];
    } else{
        NSString* errMsg = [NSString stringWithFormat: @"Opening a document failed!"];
        NSRunAlertPanel(@"Error",errMsg,nil, nil, nil);   
    }
}

- (void)printShowingPrintPanel:(BOOL)showPanels {
    NSView *printView = tableView;
    
    // enable pagination
    [[self printInfo] setHorizontalPagination: NSAutoPagination];
    [[self printInfo] setVerticalPagination: NSAutoPagination];
    // Construct the print operation and setup Print panel
    NSPrintOperation *op = [NSPrintOperation
                printOperationWithView:printView
                printInfo:[self printInfo]];
    [op setShowPanels:showPanels];
 
    // Run operation, which shows the Print panel if showPanels was YES
    [self runModalPrintOperation:op
                        delegate:nil
                  didRunSelector:NULL
                     contextInfo:NULL];
}

// ----------------------------------------
// IBActions
// ----------------------------------------

- (IBAction)setFilter:(id)sender{
    NSSearchField* searchField = (NSSearchField*)sender;
    NSString* val = [[searchField cell] stringValue];
    set_filter(documentID,[val asCString]);
    isPristine=NO;
}

- (IBAction)addColumn:(id)sender {
    NSApplication* app = [NSApplication sharedApplication];
    [app beginSheet: addColumnSheet
         modalForWindow: documentWindow
      modalDelegate: self
         didEndSelector: @selector(didEndSheet:returnCode:contextInfo:)
        contextInfo:@"CreateColumn"];
}


- (IBAction)addRow:(id)sender{
    int columnCount = number_of_columns(documentID);
    int rowCount;
    if (columnCount > 0) {
        add_row(documentID);
        [self updateChangeCount: NSChangeDone];
        [self reloadData];
        rowCount = number_of_rows(documentID);
        [tableView selectRowIndexes: [NSIndexSet indexSetWithIndex: (rowCount-1)] byExtendingSelection: NO];
        [tableView scrollRowToVisible: (rowCount-1)];
        isPristine=NO;
    } else {
        NSString* errMsg = [NSString stringWithFormat: @"This document has no columns. You cannot create a new row without at least one column."];
        NSRunAlertPanel(@"Error",errMsg,nil, nil, nil);   
    }
}

- (IBAction)toggleColumnDeleted:(id)sender{
    int colIndex=[tableView selectedColumn];
    if(colIndex>=0){
        NSTableColumn* col = (NSTableColumn*)[[tableView tableColumns] objectAtIndex: colIndex];
        NSString* label = (NSString*)[col identifier];
        toggle_column_deleted(documentID,[label asCString]);
        [self updateChangeCount: NSChangeDone];
        [self reloadData];
        [tableView deselectAll: self];
        isPristine=NO;
    }
}

- (IBAction)toggleRowDeleted:(id)sender{
    int rowIndex=[tableView selectedRow];
    if(rowIndex>=0){
        toggle_row_deleted(documentID,rowIndex);
        [self updateChangeCount: NSChangeDone];
        [self reloadData];
        [tableView deselectAll: self];
        isPristine=NO;
    }
}

- (IBAction)toggleShowDeleted:(id)sender{toggle_show_deleted(documentID);}

- (IBAction)emptyTrash:(id)sender{
    empty_trash(documentID);
    [self updateTrash];
    isPristine=NO;
}

- (IBAction)renameColumn:(id)sender{
    NSApplication* app = [NSApplication sharedApplication];
    [app beginSheet: addColumnSheet
         modalForWindow: documentWindow
      modalDelegate: self
         didEndSelector: @selector(didEndSheet:returnCode:contextInfo:)
        contextInfo:@"RenameColumn"];
}

// ----------------------------------------
// Data source methods
// ----------------------------------------

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView{
    int rowCount = number_of_rows(documentID);
    return rowCount;
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex{
    NSString* result;
    result = (NSString*)value_for_cell(documentID,[[aTableColumn identifier] asCString],rowIndex);
    return result;
}

- (void)tableView:(NSTableView *)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn *)aTableColumn 
              row:(NSInteger)rowIndex{
    NSString* val = (NSString*)anObject;
    set_value_for_cell(documentID,[[aTableColumn identifier] asCString],rowIndex,[val asCString]);
    isPristine=NO;
}


// ----------------------------------------
// NSToolbarItemValidation  methods
// ----------------------------------------

- (BOOL)validateToolbarItem:(NSToolbarItem *)theItem{ 
    int selectedIndex = -1;
    BOOL enable = NO;
    NSString* ident = theItem.itemIdentifier;
    if([ident isEqualToString: @"add-row-button"]){
        enable= YES;
    } else if ([ident isEqualToString: @"remove-row-button"]){
        selectedIndex=[tableView selectedRow];
        if (selectedIndex<0) {
            enable=NO;
        } else {
            BOOL is_deleted = is_row_deleted(documentID,selectedIndex);
            if (is_deleted){
                [theItem setLabel: @"Undelete Row"];
            }else{
                [theItem setLabel: @"Delete Row"];
            }
            enable=YES;
        }
    } else if ([ident isEqualToString: @"add-column-button"]){
        enable= YES;
    } else if ([ident isEqualToString: @"remove-column-button"]){
        selectedIndex=[tableView selectedColumn];
        if (selectedIndex<0) {
            enable=NO;
        } else {
            NSArray* cols = [tableView tableColumns];
            NSTableColumn* col = (NSTableColumn*)[cols objectAtIndex:selectedIndex];
            NSString* ident = [col identifier];
            BOOL is_deleted = is_column_deleted(documentID,[ident asCString]);
            if (is_deleted){
                [theItem setLabel: @"Undelete Col"];
            }else{
                [theItem setLabel: @"Delete Col"];
            }
            enable=YES;
        }
    } else {
        enable = NO;
    }
    return enable;
}

// ----------------------------------------
// NSTableView delegate methods
// ----------------------------------------

- (void)tableView:(NSTableView *)aTableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex{
    BOOL isColDeleted = is_column_deleted(documentID,[[aTableColumn identifier] asCString]);
    BOOL isRowDeleted = is_row_deleted(documentID, rowIndex);
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

- (void)tableViewColumnDidMove:(NSNotification *)aNotification{
    NSTableView* theTable = (NSTableView*)[aNotification object];
    NSDictionary* userInfo = [aNotification userInfo];
    NSNumber* newIndex = (NSNumber*)[userInfo objectForKey: @"NSNewColumn"];
    int new_index = [newIndex intValue];
    NSArray* cols = [theTable tableColumns];
    NSTableColumn* theCol = (NSTableColumn*)[cols objectAtIndex: new_index];
    NSString* theLabel = [theCol identifier];
    move_column(documentID,[theLabel asCString],new_index);
    [self updateChangeCount: NSChangeDone];
}

- (void)tableViewColumnDidResize:(NSNotification *)aNotification{
    NSDictionary* userInfo = [aNotification userInfo];
    NSNumber* oldWidth = (NSNumber*)[userInfo objectForKey: @"NSOldWidth"];
    int old_width = [oldWidth intValue];
    NSTableColumn* theCol = (NSTableColumn*)[userInfo objectForKey: @"NSTableColumn"];
    NSString* theLabel = [theCol identifier];
    CGFloat fWidth = [theCol width];
    int new_width = (int)fWidth;
    set_column_width(documentID,[theLabel asCString],new_width);
    [self updateChangeCount: NSChangeDone];
}

// ----------------------------------------
// Sheet delegate methods
// ----------------------------------------

- (IBAction)closeAddColumnSheet: (id)sender
{
    NSApplication* app = [NSApplication sharedApplication];
    [app endSheet:addColumnSheet];
}
	
- (void)didEndSheet:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo {
    int columnCount;
    NSString* context = (NSString*)contextInfo;
    [sheet orderOut:self];
    NSString* newLabel = [addColumnLabelField stringValue];
    // what if the user picked an empty label?
    if ([self isEmptyString: newLabel]) {
        NSRunAlertPanel(@"Change Columns",@"Can't use an empty column label!",nil, nil, nil);
        return;
    }
    // what if the user picked a duplicate label?
    if ([self isDuplicateLabel: newLabel]) {
        NSString* errMsg = [NSString stringWithFormat:@"There's already a column named '%@'!",newLabel];
        NSRunAlertPanel(@"Change Columns",errMsg,nil, nil, nil);
        return;
    }
    // otherwise, we're okay; go ahead and handle the addition or rename
    if([context isEqualToString:@"CreateColumn"]){
        add_column(documentID,[newLabel asCString]);
        isPristine=NO;
        [self reloadData];
        columnCount = number_of_columns(documentID);
        [tableView selectColumnIndexes: [NSIndexSet indexSetWithIndex: (columnCount-1)] byExtendingSelection: NO];
        [tableView scrollColumnToVisible: (columnCount-1)];
    }else if([context isEqualToString: @"RenameColumn"]){
        int col_index = [tableView selectedColumn];
        NSArray* cols = [tableView tableColumns];
        if(col_index>=0){
            NSTableColumn* col = (NSTableColumn*)[cols objectAtIndex: col_index];
            NSString* oldLabel = [col identifier];
            NSCell* hcell = [col headerCell];
            [col setIdentifier: newLabel];
            [hcell setStringValue: newLabel];
            rename_column(documentID,[oldLabel asCString],[newLabel asCString]);
            [self updateChangeCount: NSChangeDone];
        }else{
            NSString* errMsg = [NSString stringWithFormat:@"Tried to change a nonexistent column!"];
            NSRunAlertPanel(@"Change Columns",errMsg,nil, nil, nil);
            return;
        }
    }else{
        NSString* errMsg = [NSString stringWithFormat:@"Unrecognized command when changing a column"];
        NSRunAlertPanel(@"Change Columns",errMsg,nil, nil, nil);
        return;
    }
}



@end
