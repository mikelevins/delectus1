//
//  Utilities.swift
//  Delectus 2
//
//  Created by mikel evins on 10/5/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation

func applicationSupportURL () -> URL? {
    return FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first
}


