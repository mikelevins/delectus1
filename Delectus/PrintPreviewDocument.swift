//
//  PrintPreviewDocument.swift
//  Delectus
//
//  Created by mikel evins on 6/29/20.
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
    
    // Accessors
    
    func deletedItemsAreShown () -> Bool {
        let state = self.showDeletedButton.state
        if (state == NSControl.StateValue.off) {
            return false
        } else {
            return true
        }
    }
    
    // font changes
    
    func setFont(newFont: NSFont) {
        let delegate = NSApp.delegate as? DelectusDelegate
        delegate?.setContentFont(newFont)
        let cols = self.tableView.tableColumns
        let colcount = cols.count
        for i in 0..<colcount {
            let col = cols[i]
            let cell = col.dataCell as? NSCell
            cell?.font = newFont
        }
        self.tableView.rowHeight = newFont.pointSize * 1.8
        self.tableView.setNeedsDisplay()
    }

    // IBActions
    @IBAction func changeFont(sender: NSFontManager) {
        let delegate = NSApp.delegate as? DelectusDelegate
        let oldFont = delegate!.contentFont()!
        let newFont = sender.convert(oldFont)
        self.setFont(newFont: newFont)
    }
    
    @IBAction func setFilter(sender: Any) {
        let sortColumn = self.dataSource.sortColumn()
        let sortOrder = self.dataSource.sortOrder()
        let includeDeleted = self.dataSource.includeDeleted()
        let filterText = self.filterField.stringValue
        self.dataSource.getViewIncludingDeleted(includeDeleted, withSortColumn: sortColumn!, andSortOrder: sortOrder, andFilterText: filterText)
        self.tableView.reloadData()
        self.itemCountField.stringValue = String(format: "%@%ld items", self.tableView.numberOfRows)
    }
    
    @IBAction func toggleShowDeleted(sender: Any) {}
    @IBAction func performShowDeletedClick(sender: Any) {}

}
