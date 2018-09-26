//
//  DelectusDataCell.h
//  Delectus
//
//  Created by mikel on 3/31/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface DelectusDataCell : NSTextFieldCell {
@private
    BOOL isRenderingDeleted;
    NSColor* deletedHighlightColor;
}

- (BOOL)isRenderingDeleted;
- (void)setIsRenderingDeleted:(BOOL)yesOrNo;
- (NSColor *)highlightColorWithFrame:(NSRect)cellFrame inView:(NSView *)controlView;

@end
