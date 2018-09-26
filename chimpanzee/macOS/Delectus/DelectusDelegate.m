//
//  DelectusDelegate.m
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "DelectusDelegate.h"
#import "DelectusDataSource.h"
#define ___VERSION 408009
#include "gambit.h"
#include "DelectusApp.h"


@implementation DelectusDelegate

- (id)init
{
    self = [super init];
    if (self) {
        [[NSUserDefaults standardUserDefaults] synchronize];
        float fontSize = [[NSUserDefaults standardUserDefaults] floatForKey:@"DelectusContentFontSize"];
        if(fontSize==0){
            contentFont = [[NSFont userFontOfSize:12.0]retain];
        }else{
            contentFont = [[NSFont userFontOfSize:fontSize] retain];
        }

        NSURL* dbURL = [self initDatabase];
    
        if (dbURL) {
            delectusDB = [FMDatabase databaseWithURL:dbURL];
            if (delectusDB){
                BOOL isOpen = [delectusDB open];
                if (isOpen) {
                    NSLog(@"created delectus DB");
                } else {
                    NSLog(@"ERROR: failed to open delectus DB");
                }
                
                BOOL isClosed = [delectusDB close];
                if (isClosed) {
                    NSLog(@"closed delectus DB");
                } else {
                    NSLog(@"ERROR: failed to close delectus DB");
                }

            } else {
                NSLog(@"ERROR: failed to create delectus DB");
            }
        } else {
            NSLog(@"failed to retrieve database URL");
        }
        return self;
        
    }
}

- (NSURL*)initDatabase
{
    NSURL* dbURL = nil;
    NSURL* dirPath = nil;
    databaseName = @"delectusDB.sqlite3";

    
    NSString* bundleID = [[NSBundle mainBundle] bundleIdentifier];
    NSFileManager*fm = [NSFileManager defaultManager];
    
    // Find the application support directory in the home directory.
    NSArray* appSupportDir = [fm URLsForDirectory:NSApplicationSupportDirectory inDomains:NSUserDomainMask];
    
    if ([appSupportDir count] > 0)
    {
        // Append the bundle ID to the URL for the Application Support directory
        dirPath = [[appSupportDir objectAtIndex:0] URLByAppendingPathComponent:bundleID];
        
        // If the directory does not exist, this method creates it.
        NSError*    theError = nil;
        if ([fm createDirectoryAtURL:dirPath withIntermediateDirectories:YES
                           attributes:nil error:&theError])
        {
            dbURL = [dirPath URLByAppendingPathComponent:databaseName];
            return dbURL;
        } else {
            // creating the database directory failed
            NSLog(@"failed to create the Delectus Database directory in ~/Library/Application Support/");
            return nil;
        }
    }
    
}

- (void)dealloc
{
    [super dealloc];
}

- (DelectusDataSource*)newDelectus{
    DelectusDataSource* src = [[[DelectusDataSource alloc] initWithDocumentID:0] retain];
    return src;
}

- (DelectusDataSource*)readDelectusFile:(NSURL*)url{
    NSString* urlPath = [url path];
    char* path = (char*)[urlPath cStringUsingEncoding:NSASCIIStringEncoding];
    int docid = read_delectus_file(path);
    if(docid<0){
        // error! unable to read the file
        return nil;
    }else{
        DelectusDataSource* src = [[[DelectusDataSource alloc] initWithDocumentID:docid] autorelease];
        return src;
    }
}

- (DelectusDataSource*)readCSVFile:(NSURL*)url{
    NSString* urlPath = [url path];
    char* path = (char*)[urlPath cStringUsingEncoding:NSASCIIStringEncoding];
    int docid = read_delectus_csv(path);
    if(docid<0){
        // error! unable to read the file
        return nil;
    }else{
        DelectusDataSource* src = [[[DelectusDataSource alloc] initWithDocumentID:docid] autorelease];
        return src;
    }
}

- (void)setContentFont:(NSFont*)newFont{
    contentFont = newFont;
    CGFloat size = [contentFont pointSize];
    [[NSUserDefaults standardUserDefaults] setFloat:size forKey:@"DelectusContentFontSize"];    
    [[NSUserDefaults standardUserDefaults] synchronize];    
}

- (NSFont*)contentFont{
    return contentFont;
}

- (void)updateUIForDocument:(DelectusDocument*)doc withSelectedColumn:(NSInteger)colIndex andRow:(NSInteger)rowIndex{
    BOOL includeDeleted = [doc deletedItemsAreShown];
    if(includeDeleted){
        [showDeletedItemsMenu setTitle:@"Hide Deleted Items"];
    }else{
        [showDeletedItemsMenu setTitle:@"Show Deleted Items"];
    }
    
    if(rowIndex>(-1)){
        [deleteRowMenu setEnabled:YES];
        BOOL isRowDeleted = [[doc dataSource] isRowDeleted:rowIndex];
        if(isRowDeleted){
            [deleteRowMenu setTitle:@"Undelete Row"];
        }else{
            [deleteRowMenu setTitle:@"Delete Row"];
        }
    }else{
        [deleteRowMenu setTitle:@"Delete Row"];
        [deleteRowMenu setEnabled:NO];
    }
    

    if(colIndex>(-1)){
        NSTableColumn* col = [[[doc tableView] tableColumns] objectAtIndex: colIndex];
        NSString* label = [col identifier];
        [deleteColumnMenu setEnabled:YES];
        [renameColumnMenu setEnabled:YES];
        BOOL isColumnDeleted = [[doc dataSource] isColumnDeleted:label];
        if(isColumnDeleted){
            [deleteColumnMenu setTitle:@"Undelete Column"];
        }else{
            [deleteColumnMenu setTitle:@"Delete Column"];
        }
    }else{
        [renameColumnMenu setEnabled:NO];
        [deleteColumnMenu setTitle:@"Delete Column"];
        [deleteColumnMenu setEnabled:NO];
    }
    
    BOOL hasDeleted = [[doc dataSource] hasDeleted];
    if(hasDeleted){
        [purgeDeletedItemsMenu setEnabled:YES];
    }else{
        [purgeDeletedItemsMenu setEnabled:NO];
    }
}


@end
