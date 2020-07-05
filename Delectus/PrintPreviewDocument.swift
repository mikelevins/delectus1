//
//  PrintPreviewDocument.swift
//  Delectus
//
//  Created by mikel evins on 7/5/20.
//

import Foundation
import Cocoa

@objc class PrintPreviewDocument : NSDocument, NSTableViewDelegate {
    @IBOutlet weak var documentWindow: NSWindow!
    @IBOutlet weak var tableView: NSTableView!
    @IBOutlet weak var chooseColumnsLabel: NSTextField!
    @IBOutlet weak var showDeletedLabel: NSTextField!
    @IBOutlet weak var tableScrollView: NSScrollView!
    @IBOutlet weak var itemCountField: NSTextField!
    @IBOutlet weak var filterField: NSSearchField!
    @IBOutlet weak var dataSource: DelectusDataSource!
    @IBOutlet weak var showDeletedButton: NSButton!
    var columnInfo: NSMutableDictionary!
    
    // IBActions
    @IBAction func changeFont(sender: Any) {}
    @IBAction func setFilter(sender: Any) {}
    @IBAction func toggleShowDeleted(sender: Any) {}
    @IBAction func performShowDeletedClick(sender: Any) {}

}
