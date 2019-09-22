//
//  DelectusDelegate.m
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "DelectusDelegate.h"
#import "DelectusDataSource.h"
#define ___VERSION 409003
#include "gambit.h"
#include "Delectus.h"
#include <CouchbaseLite/CouchbaseLite.h>


@implementation DelectusDelegate

#pragma mark - Initialization
// --------------------------------------------------------------------------------
// Initialization
// --------------------------------------------------------------------------------

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
        // run a quick smoke test of CouchBase Lite
        //[self testCBL];
    }
    return self;
}


#pragma mark - Finalization
// --------------------------------------------------------------------------------
// Finalization
// --------------------------------------------------------------------------------

- (void)dealloc
{
    [super dealloc];
}


#pragma mark - Document lifecycle
// --------------------------------------------------------------------------------
// Document lifecycle
// --------------------------------------------------------------------------------

- (NSURL*)applicationDataDirectory {
    NSFileManager* sharedFM = [NSFileManager defaultManager];
    NSArray* possibleURLs = [sharedFM URLsForDirectory:NSApplicationSupportDirectory
                                             inDomains:NSUserDomainMask];
    NSURL* appSupportDir = nil;
    NSURL* appDirectory = nil;
    
    if ([possibleURLs count] >= 1) {
        // Use the first directory (if multiple are returned)
        appSupportDir = [possibleURLs objectAtIndex:0];
    }
    
    // If a valid app support directory exists, add the
    // app's bundle ID to it to specify the final directory.
    if (appSupportDir) {
        NSString* appBundleID = [[NSBundle mainBundle] bundleIdentifier];
        appDirectory = [appSupportDir URLByAppendingPathComponent:appBundleID];
    }
    
    return appDirectory;
}

- (DelectusDataSource*)newDelectus{
    DelectusDataSource* src = [[[DelectusDataSource alloc] initWithDocumentID:0] retain];
    return src;
}

- (DelectusDataSource*)readDelectus1File:(NSURL*)url{
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


#pragma mark - UI
// --------------------------------------------------------------------------------
// Document UI
// --------------------------------------------------------------------------------

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


#pragma mark - Tests
// --------------------------------------------------------------------------------
// Tests
// --------------------------------------------------------------------------------


// ensure CouchBaseLite functions as expected
- (void)testCBL
{
    NSURL *appSupportPath = [self applicationDataDirectory];
    CBLDatabaseConfiguration *conf = [[CBLDatabaseConfiguration alloc] init];
    NSString *path = [appSupportPath path];
    [conf setDirectory:path];
    NSError *error;
    CBLDatabase *database = [[CBLDatabase alloc]
                             initWithName:@"TestCBLDB"
                             config:conf
                             error:&error];
    NSLog(@"created CouchBase Lite Database at %@", path);
    // Create a new document (i.e. a record) in the database.
    CBLMutableDocument *mutableDoc = [[CBLMutableDocument alloc] init];
    [mutableDoc setString:@"TestCBLdoc" forKey:@"identifier"];
    [mutableDoc setString:@"test_data" forKey:@"type"];
    NSLog(@"created a new document with identifier == \"TestCBLdoc\" and type == \"test_data\"");
    // Save it to the database.
    [database saveDocument:mutableDoc error:&error];
    if (error) {
        NSLog(@"error saving the document: %@", error);
    } else {
        NSLog(@"saved the new document at %@", path);
    }
    // Read it back
    NSLog(@"Attempting to read the  document from %@", path);
    CBLDocument *foundDoc = [database documentWithID:mutableDoc.id];
    NSLog(@"found the documentat %@", path);
    NSLog(@"found type = %@", [foundDoc stringForKey:@"type"]);
    NSLog(@"found identifier = %@", [foundDoc stringForKey:@"identifier"]);
}



@end
