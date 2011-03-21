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
    if (docid==VAL_NO_DOCUMENT){
        // no document; try to make a new one
        int newID=new_delectus();
        if (newID==VAL_NO_DOCUMENT){
            // failed; give up and return nil
            return nil;
        }else{
            // succeeded; init the datasource and return it
            self = [super init];
            if (self) {
                documentID=newID;
                return self;
            } else {
                return nil;
            }
        }
    }else{
        // a delectus document was provided; init a new datasource with it
        self = [super init];
        if (self) {
            documentID=docid;
            return self;
        } else {
            return nil;
        }
    }
    return nil;
}

- (void)dealloc
{
    [super dealloc];
}

- (int)documentID{
    return documentID;
}

// --------------------------------------------------------------
// Delectus API
// --------------------------------------------------------------

- (NSString*)convertToNSString:(char*)s{
    NSString* vStr;
    if(s != NULL){
        vStr = [[NSString stringWithCString:s encoding:NSASCIIStringEncoding] retain];
        ___release_string(s);        
    }else{
        vStr=nil;
    }
    return vStr;
}

- (NSString*)version{
    char* v= version();
    NSString* result = [self convertToNSString:v];
    return result;
}

- (int)getViewIncludingDeleted:(BOOL)yesOrNo withSortColumn:(NSString*)label andSortOrder:(int)order andFilterText:(NSString*)text{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        char* sortcol = (char*)[label cStringUsingEncoding:NSASCIIStringEncoding];
        char* filtertext = (char*)[text cStringUsingEncoding:NSASCIIStringEncoding];
        int result = get_view(documentID, (int)yesOrNo, sortcol, order, filtertext);
        return result;
    }
}

- (int)countColumns{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        int result = count_columns(documentID);
        return result;
    }
}

- (NSString*)columnAtIndex:(int)index{
    if (documentID==VAL_NO_DOCUMENT){
        return nil;
    }else{
        char* lbl=column_at_index(documentID,index);
        NSString* result = [self convertToNSString:lbl];
        return result;
    }
}

- (int)countRows{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        int result = count_rows(documentID);
        return result;
    }
}

- (NSString*)valueAtColumn:(NSString*)label andRow:(int)index{
    if (documentID==VAL_NO_DOCUMENT){
        return nil;
    }else{
        char* colname = (char*)[label cStringUsingEncoding:NSASCIIStringEncoding];    
        char* val=value_at(documentID,colname,index);
        NSString* result = [self convertToNSString:val];
        return result;
    }
}

- (int)putValue:(NSString*)valStr atColumn:(NSString*)label andRow:(NSInteger)index{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        char* colname = (char*)[label cStringUsingEncoding: NSASCIIStringEncoding];    
        char* val=(char*)[valStr cStringUsingEncoding: NSASCIIStringEncoding];
        int result = put_value_at(documentID,colname,(int)index,val);
        return result;
    }
}

- (BOOL)isRowFinished:(int)index{
    if (documentID==VAL_NO_DOCUMENT){
        return NO;
    }else{
        int answer = is_row_finished(documentID,index);
        return (BOOL)answer;
    }
}

- (int)markRow:(int)index finished:(BOOL)yesOrNo{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        int result = mark_row_finished(documentID,index,(int)yesOrNo);
        return result;
    }
}

- (int)addRow{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        int result = add_row(documentID);
        return result;
    }
}

- (int)addColumn:(NSString*)label{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        char* colname = (char*)[label cStringUsingEncoding: NSASCIIStringEncoding];    
        int result = add_column(documentID,colname);
        return result;
    }
}

- (BOOL)isColumnDeleted:(NSString*)label{
    if (documentID==VAL_NO_DOCUMENT){
        return NO;
    }else{
        char* colname = (char*)[label cStringUsingEncoding: NSASCIIStringEncoding];    
        int result = is_column_deleted(documentID,colname);
        return (BOOL)result;
    }
}

- (int)markColumn:(NSString*)label deleted: (BOOL)yesOrNo{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        char* colname = (char*)[label cStringUsingEncoding: NSASCIIStringEncoding];    
        int result = mark_column_deleted(documentID,colname,yesOrNo);
        return result;
    }
}

- (BOOL)columnHasTotal:(NSString*)label{
    if (documentID==VAL_NO_DOCUMENT){
        return NO;
    }else if([label isEqualTo:@"#"]){
        return NO;
    }else if([label isEqualTo:@"?"]){
        return NO;
    }else{
        char* colname = (char*)[label cStringUsingEncoding: NSASCIIStringEncoding];    
        int result = column_has_total(documentID,colname);
        return (BOOL)result;
    }
}

- (double)columnTotal:(NSString*)label{
    if (documentID==VAL_NO_DOCUMENT){
        return 0.0;
    }else if([label isEqualTo:@"#"]){
        return 0.0;
    }else if([label isEqualTo:@"?"]){
        return 0.0;
    }else{
        char* colname = (char*)[label cStringUsingEncoding: NSASCIIStringEncoding];    
        double result = column_total(documentID,colname);
        return result;
    }
}

- (BOOL)isRowDeleted:(int)index{
    if (documentID==VAL_NO_DOCUMENT){
        return NO;
    }else{
        int result = is_row_deleted(documentID,index);
        return (BOOL)result;
    }
}

- (int)markRow:(int)index deleted:(BOOL)yesOrNo{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        int result = mark_row_deleted(documentID,index,yesOrNo);
        return result;
    }
}

- (int)compact{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        int result = compact_delectus(documentID);
        return result;
    }
}

- (int)writeDelectusFile:(NSURL*)url{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        NSString* pathStr = [url path];
        char* path = (char*)[pathStr cStringUsingEncoding: NSASCIIStringEncoding];
        int result = write_delectus_file(documentID,path);
        return result;
    }
}

- (int)writeDelectusCSV:(NSURL*)url{
    if (documentID==VAL_NO_DOCUMENT){
        return ERR_NO_DOCUMENT;
    }else{
        NSString* pathStr = [url path];
        char* path = (char*)[pathStr cStringUsingEncoding: NSASCIIStringEncoding];
        NSLog(@"\ndocumentID==%d\npath==%s",documentID,path);
        int result = write_delectus_csv(documentID,path);
        NSLog(@"\nresult==%d",result);
        return result;
    }
}

// --------------------------------------------------------------
// DataSource utils
// --------------------------------------------------------------

- (NSArray*)collectColumns{
    if (documentID==VAL_NO_DOCUMENT){
        return nil;
    }else{
        int colcount = count_columns(documentID);
        if(colcount<0){
            return (NSArray*)nil;
        }else {
            NSMutableArray* cols = [NSMutableArray array];
            if(colcount==0){
                return (NSArray*)[cols retain];
            }else{
                char* lbl;
                for(int i=0;i<colcount;i++){
                    lbl = column_at_index(documentID,i);
                    if(lbl!=NULL){
                        NSString* label = [NSString stringWithCString:lbl encoding: NSASCIIStringEncoding];
                        [cols addObject:label];
                    }
                }
                return (NSArray*)[cols retain];
            }
        }
    }
}

// --------------------------------------------------------------
// NSTableViewDataSource Protocol
// --------------------------------------------------------------


- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView{
    NSInteger count;
    if (documentID==VAL_NO_DOCUMENT){
        // new empty, unregistered document
        count = 0;
    } else {
        count = [self countRows];
    }
    return count;
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex{
    NSString* result;
    if (documentID==VAL_NO_DOCUMENT){
        result=nil;
    } else {
        NSString* label = (NSString*)[aTableColumn identifier];
        if ([label isEqualTo:@"#"]){
            result = [NSString stringWithFormat: @"%d",rowIndex];
        }else if([label isEqualTo:@"?"]){
            int index = (int)rowIndex;
            BOOL isFinished = [self isRowFinished: index];
            NSNumber* result = [NSNumber numberWithBool:isFinished];
            return result;
        }else{
            NSString* val = [self valueAtColumn:label andRow:rowIndex];
            result = val;
        }
    }
    return result;    
}

- (void)tableView:(NSTableView *)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex{
    NSString* valStr = (NSString*)anObject;
    NSString* label = (NSString*)[aTableColumn identifier];
    if([label isEqualTo:@"?"]){
        BOOL yesOrNo = [anObject boolValue];
        [self markRow:rowIndex finished:yesOrNo];
    }else{
        [self putValue:valStr atColumn:label andRow:rowIndex];
    }
}



@end
