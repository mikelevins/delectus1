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

func createURLPath(_ url: URL) -> URL? {
    do {
        try FileManager.default.createDirectory(atPath: url.path, withIntermediateDirectories: true, attributes: nil)
        return url
    } catch {
        print("Failed to create file or directory at path \(url.path)")
        return nil
    }
}


func urlPathExists(_ url: URL) -> Bool {
    return FileManager.default.fileExists(atPath: url.path)
}


