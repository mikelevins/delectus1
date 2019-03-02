//
//  DelectusDataCell.m
//  Delectus
//
//  Created by mikel on 3/31/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "DelectusDataCell.h"


@implementation DelectusDataCell

- (id)init
{
    self = [super init];
    if (self) {
        isRenderingDeleted=NO;
        deletedHighlightColor = [[NSColor colorWithCalibratedHue:0.0 saturation:1. brightness:1.0 alpha:1.0] retain];
    }
    
    return self;
}

- (void)dealloc
{
    [super dealloc];
}

- (BOOL)isRenderingDeleted{
    return isRenderingDeleted;
}

- (void)setIsRenderingDeleted:(BOOL)yesOrNo{
    isRenderingDeleted=yesOrNo;
}

- (NSColor *)highlightColorWithFrame:(NSRect)cellFrame inView:(NSView *)controlView{
    NSColor * superColor = [super highlightColorWithFrame:cellFrame inView:controlView];
    if([self isRenderingDeleted]){
        return deletedHighlightColor;
    }else{
        return superColor;
    }
}

@end
