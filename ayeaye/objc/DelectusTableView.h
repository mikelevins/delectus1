// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          DelectusTableView.h
// Project:       Delectus
// Purpose:       Custom table view
// Author:        mikel evins
//
// ***********************************************************************

#import <Cocoa/Cocoa.h>

@interface DelectusTableView : NSTableView {
}

- (void)highlightSelectionInClipRect:(NSRect)clipRect;
- (void)adjustPageHeightNew:(float *)newBottom top:(float)top bottom:(float)proposedBottom limit:(float)bottomLimit;
- (void)adjustPageWidthNew:(CGFloat *)newRight left:(CGFloat)left right:(CGFloat)proposedRight limit:(CGFloat)rightLimit;
- (void)drawPageBorderWithSize:(NSSize)borderSize;


@end
