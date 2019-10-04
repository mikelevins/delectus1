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

func printLocalStoreMetadata (metadoc: Document) {
    print("Delectus local store metadata:")
    
    if let format = metadoc.string(forKey: "format_version") {
        print("  format_version: ", format)
    } else {
        print("  format_version: <missing>")
    }
    
    if let created = metadoc.date(forKey: "created") {
        print("  created: ", created)
    } else {
        print("  created: <missing>")
    }
    
    if let modified = metadoc.date(forKey: "modified") {
        print("  modified: ", modified)
    } else {
        print("  modified: <missing>")
    }
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
                printLocalStoreMetadata(metadoc: metadoc)
            } else {
                let metadoc = makeStoreDBMetadata()
                print("saving new metadata document...")
                try db.saveDocument(metadoc)
                print("new metadata saved")
                printLocalStoreMetadata(metadoc: metadoc)
            }
            return db
        } catch {
            fatalError("Can't create the local Delectus database")
        }
    } else {
        fatalError("Can't locate the local Delectus database")
    }
}
