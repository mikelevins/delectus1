//
//  DelectusTotalsDataSource.m
//  Delectus
//
//  Created by mikel on 3/19/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "DelectusTotalsDataSource.h"
#import "DelectusDataSource.h"


@implementation DelectusTotalsDataSource

- (id)initWithDataSource: (DelectusDataSource*)src
{
    self = [super init];
    if (self) {
        dataSource = src;
    }
    
    return self;
}

- (void)dealloc
{
    [super dealloc];
}

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView{
    return 1;
}

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex{
    NSString* label = (NSString* )[aTableColumn identifier];
    if([dataSource columnHasTotal:label]){
        double total = [dataSource columnTotal:label];
        NSNumber* result = [NSNumber numberWithDouble: total];
        return result;
        return nil;
    }else{
        return nil;
    }
}


@end
