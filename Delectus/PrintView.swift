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
