//
//  DxCollection.swift
//  Delectus 2
//
//  Created by mikel evins on 10/1/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation

class DxCollection: NSObject {
    var name: String
    
    override var description: String {
        return "DxCollection (\(name))"
    }

    init(name: String) {
        self.name = name
        super.init()
    }
    
}

