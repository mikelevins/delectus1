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

func appSupportURL () -> URL? {
    return FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first
}

func localStoreURL () -> URL? {
    if let appSupportDir = appSupportURL() {
        return appSupportDir.appendingPathComponent(DelectusStoreDirectoryName, isDirectory: true)
    } else {
        return nil
    }
}

func getLocalStoreDirectory() -> URL? {
    if let storeURL = localStoreURL() {
        if (FileManager.default.fileExists(atPath: storeURL.path)) {
            print("Store directory exists!")
            return storeURL
        } else {
            print("Store directory does not exist; trying to create it")
            do {
                try FileManager.default.createDirectory(atPath: storeURL.path, withIntermediateDirectories: true, attributes: nil)
                print("Local store created")
                return storeURL
            } catch {
                print("Failed to create local store")
                print(error.localizedDescription);
                return nil
            }
        }
    } else {
        return nil
    }
}

