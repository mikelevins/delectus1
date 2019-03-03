// ***********************************************************************
// FILE IDENTIFICATION
//
// Name:          DelectusPrintView.m
// Project:       Delectus
// Purpose:       Custom table view for printing
// Author:        mikel evins
//
// ***********************************************************************

#import "DelectusPrintView.h"
#import "DelectusDelegate.h"
#import "DelectusDataSource.h"

@implementation DelectusPrintView


- (id)initWithFrame:(NSRect)frame withDataSource:(DelectusDataSource*)aSource andDocumentName:(NSString*)docName{
    self = [super initWithFrame:frame];
    if (self) {
        NSString* newline = [NSString stringWithFormat:@"\n"];
        NSString* tab = [NSString stringWithFormat:@"\t"];
        documentName = docName;
        [self setDataSource:aSource];
        NSRange selectedRange = [self selectedRange];
        [self insertText:documentName replacementRange:selectedRange];
        selectedRange = [self selectedRange];
        [self insertText:newline replacementRange:selectedRange];
        NSArray* cols = [dataSource collectColumns];
        int colcount = [cols count];
        if(colcount>0){
            int rowcount = [dataSource countRows];
            if(rowcount>0){
                for(int j=0;j<rowcount;j++){
                    NSString* colhead = [cols objectAtIndex:0];
                    selectedRange = [self selectedRange];
                    [self insertText:newline replacementRange:selectedRange];
                    selectedRange = [self selectedRange];
                    [self insertText:newline replacementRange:selectedRange];
                    NSString* index = [NSString stringWithFormat:@"%d. ",(j+1)];
                    NSString* val = [dataSource valueAtColumn:colhead andRow:j];
                    selectedRange = [self selectedRange];
                    [self insertText:index replacementRange:selectedRange];
                    selectedRange = [self selectedRange];
                    [self insertText:val replacementRange:selectedRange];
                   if(colcount>1){
                        for(int i=1;i<colcount;i++){
                            colhead = [cols objectAtIndex:i];
                            val = [dataSource valueAtColumn:colhead andRow:j];
                            if((val!=NULL)&&(![val isEqualTo:@""])){
                                selectedRange = [self selectedRange];
                                [self insertText:newline replacementRange:selectedRange];
                                selectedRange = [self selectedRange];
                                [self insertText:tab replacementRange:selectedRange];
                                selectedRange = [self selectedRange];
                                [self insertText:colhead replacementRange:selectedRange];
                                selectedRange = [self selectedRange];
                                [self insertText:@": " replacementRange:selectedRange];
                                selectedRange = [self selectedRange];
                                [self insertText:val replacementRange:selectedRange];
                            }
                        }
                    }
                }
            }else{
                NSString* col = [cols objectAtIndex:0];
                [self insertText:col];
                if(colcount>1){
                    for(int i=1;i<colcount;i++){
                        [self insertText:@", "];
                        NSString* col = [cols objectAtIndex:i];
                        [self insertText:col];
                    }
                }
            }
        }
        return self;
    } else {
        return nil;
    }
}

- (DelectusDataSource*)dataSource{
    return dataSource;
}

- (void)setDataSource:(DelectusDataSource*)aSource{
    dataSource=aSource;
}

@end
