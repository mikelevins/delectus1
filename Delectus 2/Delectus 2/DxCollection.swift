//
//  DxCollection.swift
//  Delectus 2
//
//  Created by mikel evins on 10/1/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation

class DxCollection: NSObject {
    var pathURL: URL
    
    init(withPathURL path: URL) {
        pathURL = path
        super.init()
    }
}

