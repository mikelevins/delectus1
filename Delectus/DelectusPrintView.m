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
        NSMutableString* contentsString = [NSMutableString string];
        documentName = docName;
        [self setDataSource:aSource];
        [contentsString appendString:documentName];
        [contentsString appendString:@"\n"];

        NSArray* cols = [dataSource collectColumns];
        NSUInteger colcount = [cols count];
        if(colcount>0){
            int rowcount = [dataSource countRows];
            if(rowcount>0){
                for(int j=0;j<rowcount;j++){
                    NSString* colhead = [cols objectAtIndex:0];
                    [contentsString appendString:@"\n\n"];

                    NSString* index = [NSString stringWithFormat:@"%d. ",(j+1)];
                    NSString* val = [dataSource valueAtColumn:colhead andRow:j];
                    [contentsString appendString:index];
                    [contentsString appendString:val];

                   if(colcount>1){
                        for(int i=1;i<colcount;i++){
                            colhead = [cols objectAtIndex:i];
                            val = [dataSource valueAtColumn:colhead andRow:j];
                            if((val!=NULL)&&(![val isEqualTo:@""])){
                                [contentsString appendString:@"\n\t"];
                                [contentsString appendString:colhead];
                                [contentsString appendString:@": "];
                                [contentsString appendString:val];
                            }
                        }
                    }
                }
            }else{
                NSString* col = [cols objectAtIndex:0];
                [contentsString appendString:col];
                
                if(colcount>1){
                    for(int i=1;i<colcount;i++){
                        NSString* col = [cols objectAtIndex:i];
                        [contentsString appendString:@", "];
                        [contentsString appendString:col];
                    }
                }
            }
        }
        [self setString:contentsString];
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
