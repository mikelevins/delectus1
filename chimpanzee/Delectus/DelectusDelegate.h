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
#import <Realm/Realm.h>

@interface DelectusDelegate : NSObject {
    NSFont* contentFont;
    IBOutlet id addRowMenu;
    IBOutlet id deleteRowMenu;
    IBOutlet id addColumnMenu;
    IBOutlet id deleteColumnMenu;
    IBOutlet id renameColumnMenu;
    IBOutlet id showDeletedItemsMenu;
    IBOutlet id purgeDeletedItemsMenu;
    RLMRealm *realm;
}

- (DelectusDataSource*)newDelectus;
- (DelectusDataSource*)readDelectusFile:(NSURL*)url;
- (DelectusDataSource*)readCSVFile:(NSURL*)url;
- (void)setContentFont:(NSFont*)newFont;
- (NSFont*)contentFont;
- (void)updateUIForDocument:(DelectusDocument*)doc withSelectedColumn:(NSInteger)colIndex andRow:(NSInteger)rowIndex;

@end
