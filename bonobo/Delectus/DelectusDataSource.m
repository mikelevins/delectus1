//
//  DelectusDataSource.m
//  Delectus
//
//  Created by mikel on 3/16/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "DelectusDataSource.h"
#define ___VERSION 406000
#include "gambit.h"
#include "Delectus.h"


@implementation DelectusDataSource

- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}

- (id)initWithDocumentID:(int)docid
{
    self = [super init];
    if (self) {
        documentID=docid;
    }
    
    return self;
}

- (void)dealloc
{
    [super dealloc];
}

- (int)documentID{
    return documentID;
}

- (void)setDocumentID:(int)docID{
    documentID=docID;
}


// --------------------------------------------------------------
// Delectus API
// --------------------------------------------------------------

- (NSString*)version{
    char* v= version();
    NSString* vStr = [[NSString stringWithCString:v encoding:NSASCIIStringEncoding] retain];
    ___release_string(v);
    return vStr;
}

+ (DelectusDataSource*)newDocument{
    int docID = new_delectus();
    DelectusDataSource* src = [[DelectusDataSource alloc] init];
    [src setDocumentID: docID];
    return src;
}

+ (DelectusDataSource*)readDelectusFile:(NSURL*)url{
    NSString* pathStr = [url absoluteString];
    char* path = (char*)[pathStr cStringUsingEncoding: NSASCIIStringEncoding];
    int docID = read_delectus_file(path);
    DelectusDataSource* src = [[DelectusDataSource alloc] init];
    [src setDocumentID: docID];
    return src;
}

+ (DelectusDataSource*)readDelectusCSV:(NSURL*)url{
    NSString* pathStr = [url absoluteString];
    char* path = (char*)[pathStr cStringUsingEncoding: NSASCIIStringEncoding];
    int docID = read_delectus_csv(path);
    DelectusDataSource* src = [[DelectusDataSource alloc] init];
    [src setDocumentID: docID];
    return src;
}

- (int)getViewIncludingDeleted:(BOOL)yesOrNo withSortColumn:(NSString*)label andSortOrder:(int)order andFilterText:(NSString*)text{return 0;}
- (int)countColumns{return 0;}
- (int)countRows{return 0;}
- (NSString*)valueAtColumn:(NSString*)label andRow:(int)index{return nil;}
- (int)putValue:(NSString*)val atColumn:(NSString*)label andRow:(int)index{return 0;}
- (BOOL)isRowFinished:(int)index{return NO;}
- (int)markRow:(int)index finished:(BOOL)yesOrNo{return 0;}
- (int)addRow{return 0;}
- (int)addColumn:(NSString*)label{return 0;}
- (BOOL)isColumnDeleted:(NSString*)label{return NO;}
- (int)markColumn:(NSString*)label deleted: (BOOL)yesOrNo{return 0;}
- (BOOL)columnHasTotal:(NSString*)label{return NO;}
- (int)columnTotal:(NSString*)label{return 0;}
- (BOOL)isRowDeleted:(int)index{return NO;}
- (int)markRow:(int)index deleted:(BOOL)yesOrNo{return 0;}
- (int)compact{return 0;}
- (int)writeDelectusFile:(NSURL*)url{return 0;}
- (int)writeDelectusCSV:(NSURL*)url{return 0;}


// --------------------------------------------------------------
// NSTableViewDataSource Protocol
// --------------------------------------------------------------


- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView{
    NSInteger count;
    if (documentID==OBJ_NO_OID){
        // new empty, unregistered document
        count = 0;
    } else {
        count = count_rows(documentID);
    }
    return count;
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex{
    return nil;
}

- (void)tableView:(NSTableView *)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex{
}



@end
