//
//  DxList.swift
//  Delectus 2
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation

class DxList: NSObject {
    var name: String

    override var description: String {
        return "DxList \(name)"
    }

    init(name: String, collection: String) {
        self.name = name
        super.init()
    }
    
}
