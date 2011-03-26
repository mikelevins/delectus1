//
//  DelectusDelegate.h
//  Delectus
//
//  Created by mikel on 3/12/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "DelectusDataSource.h"

@interface DelectusDelegate : NSObject {
    NSFont* contentFont;
}

- (DelectusDataSource*)newDelectus;
- (DelectusDataSource*)readDelectusFile:(NSURL*)url;
- (DelectusDataSource*)readCSVFile:(NSURL*)url;
- (void)setContentFont:(NSFont*)newFont;
- (NSFont*)contentFont;


@end
