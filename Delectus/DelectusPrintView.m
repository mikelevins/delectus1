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
        [self insertText:documentName replacementRange:[self selectedRange]];
        [self insertText:newline replacementRange:[self selectedRange]];
        NSArray* cols = [dataSource collectColumns];
        int colcount = [cols count];
        if(colcount>0){
            int rowcount = [dataSource countRows];
            if(rowcount>0){
                for(int j=0;j<rowcount;j++){
                    NSString* colhead = [cols objectAtIndex:0];
                    [self insertText:newline replacementRange:[self selectedRange]];
                    [self insertText:newline replacementRange:[self selectedRange]];
                    NSString* index = [NSString stringWithFormat:@"%d. ",(j+1)];
                    NSString* val = [dataSource valueAtColumn:colhead andRow:j];
                    [self insertText:index replacementRange:[self selectedRange]];
                    [self insertText:val replacementRange:[self selectedRange]];
                   if(colcount>1){
                        for(int i=1;i<colcount;i++){
                            colhead = [cols objectAtIndex:i];
                            val = [dataSource valueAtColumn:colhead andRow:j];
                            if((val!=NULL)&&(![val isEqualTo:@""])){
                                [self insertText:newline replacementRange:[self selectedRange]];
                                [self insertText:tab replacementRange:[self selectedRange]];
                                [self insertText:colhead replacementRange:[self selectedRange]];
                                [self insertText:@": " replacementRange:[self selectedRange]];
                                [self insertText:val replacementRange:[self selectedRange]];
                            }
                        }
                    }
                }
            }else{
                NSString* col = [cols objectAtIndex:0];
                [self insertText:col replacementRange:[self selectedRange]];
                if(colcount>1){
                    for(int i=1;i<colcount;i++){
                        [self insertText:@", " replacementRange:[self selectedRange]];
                        NSString* col = [cols objectAtIndex:i];
                        [self insertText:col replacementRange:[self selectedRange]];
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
