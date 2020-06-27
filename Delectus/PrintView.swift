//
//  PrintView.swift
//  Delectus
//
//  Created by mikel evins on 6/27/20.
//

import Foundation
import Cocoa

@objc class PrintView : NSTextView {
    var dataSource: DelectusDataSource?
    var documentName: NSString = ""

    init(frame aRect: NSRect, dataSource: DelectusDataSource, documentName: NSString) {
        self.documentName = documentName
        super.init(frame: aRect)
        self.setDataSource(aSource: dataSource)

        let cols: [String] = dataSource.collectColumns() as? [String] ?? []
        
        let colcount = cols.count
        let rowcount = dataSource.countRows()
        
        for j in 0..<rowcount {
            let colhead = cols[0]
            let indexStr = "\(j+1)"
            let val = dataSource.value(atColumn: colhead, andRow: j)
            print("\n",indexStr,". ", val ?? "<nil>")
            for i in 0..<colcount {
                
            }
        }
    }

    required init?(coder aDecoder: NSCoder) {
        self.dataSource = nil
        self.documentName = ""
        super.init(coder: aDecoder)
    }

    @objc func setDataSource (aSource: DelectusDataSource) {
        self.dataSource = aSource
    }
}
