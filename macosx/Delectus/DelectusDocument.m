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
	// TODO fix this to do the right thing
    return NO;
}

- (int)indexOfColumnLabel: (NSString*)aLabel{
    int i;
    NSTableColumn* col;
    NSString* lbl;
    NSInteger result = NSNotFound;
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
	// TODO: get the labels the right way
    NSArray* doc_column_labels = [NSArray array];

    // 1. clear the columns
    NSArray* view_columns = [tableView tableColumns];
    NSMutableArray* cols_to_delete = [NSMutableArray arrayWithCapacity:[view_columns count]];
    for(i=0;i<[view_columns count];i++){
        NSTableColumn* col = (NSTableColumn*)[view_columns objectAtIndex:i];
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
	// TODO get this from the db
    BOOL are_deleteds_shown = YES;
    for(i=0;i<[new_columns count];i++){
        NSTableColumn* col = (NSTableColumn*)[new_columns objectAtIndex:i];
		// TODO get this from the db
        BOOL is_deleted = NO;
        [col setMinWidth: 50.0];
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
	// TODO implement importing CSV
	return -1;
}

// returns a document ID
- (int)readDelectusFile:(NSURL *)absoluteURL {
    NSString* fPath = [absoluteURL path];
	// TODO implement reading a delectus file
    return -1;
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
    NSString* savePath;
    if([typeName isEqualToString: @"delectus"]){
        savePath = [absoluteURL path];
		// TODO implement saving delectus
        int written_docID = -1;
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
    [toggleShownLabel setStringValue: @"Deleted items are red."];
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
	// TODO implement by querying db
    BOOL deleted_items_shown = YES;
	// TODO implement by querying db
    BOOL any_deleted_items = NO;

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
	// TODO implement by asking db
    int rowCount = 1;
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
		// TODO add code to set the sort here
    } else{
        [sender editColumn:[sender clickedColumn] row:[sender clickedRow] withEvent:nil  select:YES];
    }
}

- (void)windowControllerDidLoadNib:(NSWindowController *)windowController{
	// TODO create a document here
}

- (void)printShowingPrintPanel:(BOOL)showPanels {
	// TODO implement printing
}

// ----------------------------------------
// IBActions
// ----------------------------------------

- (IBAction)setFilter:(id)sender{
    NSSearchField* searchField = (NSSearchField*)sender;
    NSString* val = [[searchField cell] stringValue];
	// TODO implement setting the filter
    //set_filter(documentID,[val asCString]);
    //isPristine=NO;
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
	// TODO implement adding a row
}

- (IBAction)toggleColumnDeleted:(id)sender{
	// TODO implement marking a column deleted
}

- (IBAction)toggleRowDeleted:(id)sender{
	// TODO implement marking a row deleted
}

- (IBAction)toggleShowDeleted:(id)sender{
	// TODO implement show/hide deleted
}

- (IBAction)emptyTrash:(id)sender{
	// TODO implement emptying trash
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
	// TODO implement by asking db
    int rowCount = 1;
    return rowCount;
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex{
	// TODO implement by asking db
    NSString* result;
    result = @"Foo";
    return result;
}

- (void)tableView:(NSTableView *)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn *)aTableColumn 
              row:(NSInteger)rowIndex{
    NSString* val = (NSString*)anObject;
	// TODO implement by asking db
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
			// TODO implement by asking db
            BOOL is_deleted = NO;
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
			// TODO implement by asking db
            BOOL is_deleted = NO;
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
	// TODO implement by asking db
    BOOL isColDeleted = NO;
	// TODO implement by asking db
    BOOL isRowDeleted = NO;
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
	// TODO implement
    //move_column(documentID,[theLabel asCString],new_index);
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
	// TODO maybe implement
    //set_column_width(documentID,[theLabel asCString],new_width);
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
		// TODO implement
        //add_column(documentID,[newLabel asCString]);
        isPristine=NO;
        [self reloadData];
		// TODO implement by asking db
        columnCount = 1;
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
			// TODO implement
            //rename_column(documentID,[oldLabel asCString],[newLabel asCString]);
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
