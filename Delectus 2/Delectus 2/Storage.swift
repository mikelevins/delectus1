//
//  DxStore.swift
//  Delectus 2
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation
import CouchbaseLiteSwift

let DelectusStoreDirectoryName = "com.mikelevins.delectus.Store"
let DelectusStoreDBName = "DelectusDB"
let DelectusStoreMetadataID = "DelectusStoreMetadata"
let DelectusStoreFormatVersion = "2.0d1"

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

func makeStoreDBMetadata () -> MutableDocument {
    let metadoc = MutableDocument(id: DelectusStoreMetadataID)
        .setString(DelectusStoreFormatVersion, forKey: "format_version")
        .setDate(Date(), forKey: "created")
        .setDate(Date(), forKey: "modified")
    return metadoc
}

func openLocalStore() -> Database? {
    if  let dataDir = getLocalStoreDirectory() {
        let dataPath = dataDir.path
        let conf = DatabaseConfiguration()
        conf.directory = dataPath
        do {
            let db = try Database(name: DelectusStoreDBName, config: conf)
            print("opened the Delectus database")
            if let metadoc = db.document(withID: DelectusStoreMetadataID) {
                print("found store metadata:")
                print("  local store format: ", metadoc.string(forKey: "format_version"))
                print("  local store created: ", metadoc.date(forKey: "created"))
                print("  local store modified: ", metadoc.date(forKey: "modified"))
                return db
            } else {
                let metadoc = makeStoreDBMetadata()
                print("created store metadata; saving...")
                try db.saveDocument(metadoc)
                print("  local store format: ", metadoc.string(forKey: "format_version"))
                print("  local store created: ", metadoc.date(forKey: "created"))
                print("  local store modified: ", metadoc.date(forKey: "modified"))
                return db
            }
        } catch {
            fatalError("Can't create the local Delectus database: failed to save the database metadata")
        }
    } else {
        fatalError("Can't locate the local Delectus database")
    }
}
