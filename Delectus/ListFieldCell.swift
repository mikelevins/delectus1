//
//  ListFieldCell.swift
//  Delectus
//
//  Created by mikel evins on 6/26/20.
//

import Foundation
import Cocoa

@objc class ListFieldCell : NSTextFieldCell {
    var isRenderingDeleted: Bool;
    var deletedHighlightColor: NSColor;
    
    override init(textCell aString: String) {
        self.isRenderingDeleted = false
        self.deletedHighlightColor = NSColor.red
        super.init(textCell: aString)
    }

    required init(coder aDecoder: NSCoder) {
        self.isRenderingDeleted = false
        self.deletedHighlightColor = NSColor.red
        super.init(coder: aDecoder)
    }
    
    @objc func setIsRenderingDeleted (yesOrNo: Bool) {
        self.isRenderingDeleted = yesOrNo
    }

    override func highlightColor(withFrame cellFrame: NSRect, in controlView: NSView) -> NSColor? {
        let superColor: NSColor = super.highlightColor(withFrame: cellFrame, in: controlView) ?? NSColor.highlightColor
        if (self.isRenderingDeleted) {
            return self.deletedHighlightColor
        } else {
            return superColor
        }
    }

}
