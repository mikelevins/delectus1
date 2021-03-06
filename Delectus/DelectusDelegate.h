//
//  DelectusDelegate.h
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DelectusDataSource.h"
#import "DelectusDocument.h"

@interface DelectusDelegate : NSObject {
    NSFont* contentFont;
    IBOutlet id addRowMenu;
    IBOutlet id deleteRowMenu;
    IBOutlet id addColumnMenu;
    IBOutlet id deleteColumnMenu;
    IBOutlet id renameColumnMenu;
    IBOutlet id showDeletedItemsMenu;
    IBOutlet id purgeDeletedItemsMenu;
}

- (NSURL*)applicationDataDirectory;
- (DelectusDataSource*)newDelectus;
- (DelectusDataSource*)readDelectus1File:(NSURL*)url;
- (DelectusDataSource*)readCSVFile:(NSURL*)url;
- (void)setContentFont:(NSFont*)newFont;
- (NSFont*)contentFont;
- (void)updateUIForDocument:(DelectusDocument*)doc withSelectedColumn:(NSInteger)colIndex andRow:(NSInteger)rowIndex;

@end
