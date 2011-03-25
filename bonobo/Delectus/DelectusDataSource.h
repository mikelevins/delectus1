//
//  DelectusDataSource.h
//  Delectus
//
//  Created by mikel on 3/16/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface DelectusDataSource : NSObject <NSTableViewDataSource> {
@private

    int documentID;

}

// --------------------------------------------------------------
// initialization
// --------------------------------------------------------------

- (id)initWithDocumentID:(int)docid;

- (int)documentID;

// --------------------------------------------------------------
// Delectus API
// --------------------------------------------------------------

- (NSString*)version;
- (int)getViewIncludingDeleted:(BOOL)yesOrNo withSortColumn:(NSString*)label andSortOrder:(int)order andFilterText:(NSString*)text;
- (int)countColumns;
- (int)countRows;
- (NSString*)valueAtColumn:(NSString*)label andRow:(int)index;
- (int)putValue:(NSString*)valStr atColumn:(NSString*)label andRow:(NSInteger)index;
- (BOOL)isRowFinished:(int)index;
- (int)markRow:(int)index finished:(BOOL)yesOrNo;
- (int)addRow;
- (int)addColumn:(NSString*)label;
- (BOOL)isColumnDeleted:(NSString*)label;
- (BOOL)isDuplicateLabel:(NSString*)label;
- (int)markColumn:(NSString*)label deleted: (BOOL)yesOrNo;
- (BOOL)columnHasTotal:(NSString*)label;
- (double)columnTotal:(NSString*)label;
- (BOOL)isRowDeleted:(int)index;
- (int)markRow:(int)index deleted:(BOOL)yesOrNo;
- (int)compact;
- (int)writeDelectusFile:(NSURL*)url;
- (int)writeDelectusCSV:(NSURL*)url;

// --------------------------------------------------------------
// utils
// --------------------------------------------------------------
- (NSArray*)collectColumns;

// --------------------------------------------------------------
// NSTableViewDataSource Protocol
// --------------------------------------------------------------

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex;
- (void)tableView:(NSTableView *)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex;

@end