// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          DelectusPrintView.h
// Project:       Delectus
// Purpose:       Custom table view for printing
// Author:        mikel evins
//
// ***********************************************************************

#import <Cocoa/Cocoa.h>
#import "DelectusDataSource.h"

@interface DelectusPrintView : NSTextView {
    DelectusDataSource* dataSource;
    NSString* documentName;
}

- (id)initWithFrame:(NSRect)frame withDataSource:(DelectusDataSource*)aSource andDocumentName:(NSString*)docName;
- (DelectusDataSource*)dataSource;
- (void)setDataSource:(DelectusDataSource*)aSource;

@end
