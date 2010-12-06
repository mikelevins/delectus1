// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          DelectusDocument.h
// Project:       Delectus
// Purpose:       Document definitions
// Author:        mikel evins
//
// ***********************************************************************

#import <Cocoa/Cocoa.h>

#define NO_DOCUMENT (-1)

@interface DelectusDocument : NSDocument {
    BOOL isPristine;
    int documentID;
    IBOutlet id documentWindow;
    IBOutlet id tableView;
    IBOutlet id addColumnSheet;
    IBOutlet id addColumnLabelField;
    IBOutlet id toggleShownButton;
    IBOutlet id toggleShownLabel;
    IBOutlet id toggleColumnDeletedButton;
    IBOutlet id toggleRowDeletedButton;
    IBOutlet id itemCountField;
}

// initialization
- (id)init;

// accessors
- (void)reloadData;
- (int)documentID;
- (BOOL)isPristine;


// NSDocument APIs
- (BOOL)readFromURL: (NSURL *)absoluteURL ofType:(NSString *) typeName error: (NSError **)outError;
- (BOOL)writeToURL:(NSURL *)absoluteURL ofType:(NSString *)typeName error:(NSError **)outError;
- (NSString *)windowNibName;
- (void)windowControllerDidLoadNib:(NSWindowController *)windowController;
- (void)printShowingPrintPanel:(BOOL)showPanels;

// IBActions
- (IBAction)setFilter:(id)sender;
- (IBAction)addColumn:(id)sender;
- (IBAction)addRow:(id)sender;
- (IBAction)toggleColumnDeleted:(id)sender;
- (IBAction)toggleRowDeleted:(id)sender;
- (IBAction)toggleShowDeleted:(id)sender;
- (IBAction)emptyTrash:(id)sender;
- (IBAction)renameColumn:(id)sender;

// DelectusDocument APIs
- (void) updateTrash;
- (void) updateItemCount;
- (void) deselectAll;

// Data source methods
- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex;
- (void)tableView:(NSTableView *)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn *)aTableColumn 
                                                               row:(NSInteger)rowIndex;

// NSToolbarItemValidation  methods
- (BOOL)validateToolbarItem:(NSToolbarItem *)theItem;

// NSTableView delegate methods
                                                                              - (void)tableView:(NSTableView *)aTableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex;

// Sheet delegate methods
- (IBAction)closeAddColumnSheet: (id)sender;
- (void)didEndSheet:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo;

@end
