//
//  DelectusDocument.h
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 mikel evins. All rights reserved.
//


#import <Cocoa/Cocoa.h>
#import "DelectusDataSource.h"

@interface DelectusDocument : NSDocument <NSTableViewDelegate>
{
    IBOutlet id documentWindow;
    IBOutlet NSTableView* tableView;
    IBOutlet id addColumnSheet;
    IBOutlet id addColumnLabelField;
    IBOutlet id tableScrollView;
    IBOutlet id itemCountField;    
    IBOutlet id filterField;    
    IBOutlet DelectusDataSource* dataSource;
    IBOutlet id addColumnButton;
    IBOutlet id delColumnButton;
    IBOutlet id addRowButton;
    IBOutlet id delRowButton;
    IBOutlet id showDeletedButton;
    NSFont* contentFont;
}

// Handle font changes

- (NSFont*)font;
- (void)setFont:(NSFont*)newFont;
- (void)changeFont:(id)sender;

// IBActions
- (IBAction)addRow:(id)sender;
- (IBAction)toggleRowDeleted:(id)sender;
- (IBAction)addColumn:(id)sender;
- (IBAction)toggleColumnDeleted:(id)sender;
- (IBAction)setFilter:(id)sender;
- (IBAction)toggleShowDeleted:(id)sender;
- (IBAction)emptyTrash:(id)sender;
- (IBAction)renameColumn:(id)sender;
- (IBAction)acceptNewColumn:(id)sender;

// handle sheets

- (void)sheetDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo;


@end