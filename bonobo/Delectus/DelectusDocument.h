//
//  DelectusDocument.h
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 mikel evins. All rights reserved.
//


#import <Cocoa/Cocoa.h>

@interface DelectusDocument : NSDocument <NSTableViewDataSource>
{
    IBOutlet id documentWindow;
    IBOutlet NSTableView* tableView;
    IBOutlet id tableScrollView;
    IBOutlet NSTableView* totalsView;
    IBOutlet id totalsScrollView;
    IBOutlet id itemCountField;    
    IBOutlet id filterField;    
    IBOutlet id dataSource;    
    IBOutlet id totalsDataSource;    
}

// IBActions
- (IBAction)toggleToDo:(id)sender;
- (IBAction)addRow:(id)sender;
- (IBAction)toggleRowDeleted:(id)sender;
- (IBAction)addColumn:(id)sender;
- (IBAction)toggleColumnDeleted:(id)sender;
- (IBAction)toggleTotals:(id)sender;
- (IBAction)setFilter:(id)sender;
- (IBAction)toggleShowDeleted:(id)sender;
- (IBAction)emptyTrash:(id)sender;
- (IBAction)renameColumn:(id)sender;



@end
