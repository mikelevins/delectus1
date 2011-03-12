//
//  DelectusDocument.h
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 mikel evins. All rights reserved.
//


#import <Cocoa/Cocoa.h>

@interface DelectusDocument : NSDocument
{
    int documentID;
    IBOutlet id documentWindow;
    IBOutlet id tableView;
    IBOutlet id totalsView;
    IBOutlet id itemCountField;    
    IBOutlet id filterField;    
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
