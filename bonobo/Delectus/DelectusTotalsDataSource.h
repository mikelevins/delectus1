//
//  DelectusTotalsDataSource.h
//  Delectus
//
//  Created by mikel on 3/19/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface DelectusTotalsDataSource : NSObject {
@private
    id dataSource;
}

- (id)initWithDataSource: (id)src;

// --------------------------------------------------------------
// NSTableViewDataSource Protocol
// --------------------------------------------------------------

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex;


@end
