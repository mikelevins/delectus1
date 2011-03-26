//
//  DelectusDelegate.m
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "DelectusDelegate.h"
#define ___VERSION 406000
#include "gambit.h"
#include "Delectus.h"


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

    }
    return self;
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


@end
