//
//  DxStore.swift
//  Delectus 2
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation

let DelectusStoreDirectoryName = "com.mikelevins.delectus.Store"
let DelectusStoreDBName = "DelectusDB"

func getAppSupportURL () -> URL {
    let urls = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)
    if let url = urls.first {
        return url
    } else {
        fatalError("Can't locate the Delectus application container")
    }
}

func getStoreURL () -> URL {
    let appSupportDir = getAppSupportURL()
    return appSupportDir.appendingPathComponent(DelectusStoreDirectoryName)
}

func createLocalStoreDirectory() -> Bool {
    let storeURL = getStoreURL()
    do {
        try FileManager.default.createDirectory(atPath: storeURL.path, withIntermediateDirectories: true, attributes: nil)
        print("Local store created")
        return true
    } catch {
        print("Failed to create local store")
        print(error.localizedDescription);
        return false
    }
}

func getLocalStoreDirectory() -> URL? {
    let url = getStoreURL()
    if (FileManager.default.fileExists(atPath: url.path)) {
        print("Store directory exists!")
        return url
    } else {
        print("Store directory does not exist; trying to create it")
        do {
            try FileManager.default.createDirectory(atPath: url.path, withIntermediateDirectories: true, attributes: nil)
            print("Store directory created at \(url.path)")
            return url
        } catch {
            print("Failed to create the store directory at \(url.path)")
            print(error.localizedDescription);
            return nil
        }
    }
}
