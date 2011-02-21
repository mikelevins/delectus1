// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          DelectusTableView.m
// Project:       Delectus
// Purpose:       Custom table view
// Author:        mikel evins
//
// ***********************************************************************

#import "DelectusTableView.h"

@implementation DelectusTableView

- (void)highlightSelectionInClipRect:(NSRect)clipRect{
    // FIXME: temporary, while I'm figuring things out
    [super highlightSelectionInClipRect:clipRect];
}

- (void)adjustPageHeightNew:(float *)newBottom top:(float)top bottom:(float)proposedBottom limit:(float)bottomLimit{
    int	 cutoffRow = [self rowAtPoint:NSMakePoint(0, proposedBottom)];
    NSRect	 rowBounds;

    if (cutoffRow == -1) {
        *newBottom = proposedBottom;
    } else {
        rowBounds = [self rectOfRow:cutoffRow];
        *newBottom = NSMinY(rowBounds);
    }
}

- (void)adjustPageWidthNew:(CGFloat *)newRight left:(CGFloat)left right:(CGFloat)proposedRight limit:(CGFloat)rightLimit{
    int	 cutoffCol = [self columnAtPoint:NSMakePoint(proposedRight,0)];
    NSRect	 colBounds;

    if (cutoffCol == -1) {
        *newRight = proposedRight;
    } else {
        colBounds = [self rectOfColumn:cutoffCol];
        *newRight = NSMinX(colBounds);
    }
}

- (void)drawPageBorderWithSize:(NSSize)borderSize{
    [super drawPageBorderWithSize:borderSize];
    NSRect frame = [self frame];


    [self setFrame:NSMakeRect(0.0, 0.0, borderSize.width, borderSize.height)];

    [self lockFocus];

    int page = [[NSPrintOperation currentOperation] currentPage];
    NSString *pageNumberString = [NSString stringWithFormat:@"- %d -", page];

    NSMutableDictionary *attribs = [NSMutableDictionary dictionary];  
    [attribs setObject:[NSFont systemFontOfSize:10.0] forKey:NSFontAttributeName];
    NSSize pageNumberSize = [pageNumberString sizeWithAttributes:attribs];

    float bottomMargin = [[[NSPrintOperation currentOperation] printInfo] bottomMargin];
    NSPoint pageNumberPoint = NSMakePoint((borderSize.width - pageNumberSize.width) / 2.0,
                                          borderSize.height - (bottomMargin + pageNumberSize.height) / 2.0);

    [[NSString stringWithFormat:@"- %d -", page] drawAtPoint:pageNumberPoint
                                              withAttributes:attribs];


    [self unlockFocus];


    [self setFrame:frame];
}

@end
