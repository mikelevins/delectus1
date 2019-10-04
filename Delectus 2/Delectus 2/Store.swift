//
//  DxStore.swift
//  Delectus 2
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation
import CouchbaseLiteSwift

// MARK: -
// MARK: store constants

let DelectusStoreDirectoryName = "com.mikelevins.delectus.Store"
let DelectusStoreDBName = "DelectusDB"
let DelectusStoreMetadataID = "DelectusStoreMetadata"
let DelectusStoreFormatVersion = "2.0d1"

// MARK: -
// MARK: store metadata

func makeStoreMetadata () -> MutableDocument {
    let metadoc = MutableDocument(id: DelectusStoreMetadataID)
        .setString(DelectusStoreFormatVersion, forKey: "format_version")
        .setDate(Date(), forKey: "created")
        .setDate(Date(), forKey: "modified")
    return metadoc
}

func printStoreMetadata (metadoc: Document) {
    print("Delectus local store metadata:")
    
    if let format = metadoc.string(forKey: "format_version") {
        print("  format_version: ", format)
    } else {
        print("  format_version: <missing>")
    }
    
    if let created = metadoc.date(forKey: "created") {
        print("         created: ", created)
    } else {
        print("         created: <missing>")
    }
    
    if let modified = metadoc.date(forKey: "modified") {
        print("        modified: ", modified)
    } else {
        print("        modified: <missing>")
    }
}


// MARK: -
// MARK: store operations

func getStoreURL () -> URL? {
    if let appSupportDir = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first {
        return appSupportDir.appendingPathComponent(DelectusStoreDirectoryName, isDirectory: true)
    } else {
        print("Failed to locate the Application SUpport directory")
        return nil
    }
}

func getStoreMetadata (db: Database) -> Document? {
    return db.document(withID: DelectusStoreMetadataID)
}

func findOrCreateStoreDirectory() -> URL? {
    if let getStoreURL = getStoreURL() {
        if (FileManager.default.fileExists(atPath: getStoreURL.path)) {
            print("Store directory exists")
            return getStoreURL
        } else {
            print("Store directory does not exist; trying to create it")
            do {
                try FileManager.default.createDirectory(atPath: getStoreURL.path, withIntermediateDirectories: true, attributes: nil)
                print("Local store created")
                return getStoreURL
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

func openStore() -> Database? {
    if  let dataDir = findOrCreateStoreDirectory() {
        let dataPath = dataDir.path
        let conf = DatabaseConfiguration()
        conf.directory = dataPath
        do {
            let db = try Database(name: DelectusStoreDBName, config: conf)
            print("opened the Delectus database")
            if let metadoc = getStoreMetadata(db: db) {
                printStoreMetadata(metadoc: metadoc)
            } else {
                let metadoc = makeStoreMetadata()
                print("creating new metadata document:")
                printStoreMetadata(metadoc: metadoc)
                try db.saveDocument(metadoc)
                print("new metadata saved")
            }
            return db
        } catch {
            fatalError("Can't create the local Delectus database")
        }
    } else {
        fatalError("Can't locate the local Delectus database")
    }
}
