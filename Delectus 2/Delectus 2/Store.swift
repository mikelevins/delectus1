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

let kDelectusStoreDBName = "DelectusDB"
let kDelectusStoreDirectoryName = "com.mikelevins.delectus.Store"
let kDelectusStoreFormatVersion = "2.0d1"
let kDelectusStoreMetadataID = "DelectusStoreMetadata"
let kMetadataKeyCreated = "created"
let kMetadataKeyFormatVersion = "format_version"
let kMetadataKeyModified = "modified"

// MARK: -
// MARK: store variables

var delectusStore: Database?

// MARK: -
// MARK: store metadata

func makeStoreMetadataDocument () -> MutableDocument {
    let metadoc = MutableDocument(id: kDelectusStoreMetadataID)
        .setString(kDelectusStoreFormatVersion, forKey: kMetadataKeyFormatVersion)
        .setDate(Date(), forKey: kMetadataKeyCreated)
        .setDate(Date(), forKey: kMetadataKeyModified)
    return metadoc
}

func printStoreMetadata (metadoc: Document) {
    let format = metadoc.string(forKey: "format_version")
    let created = metadoc.date(forKey: "created")
    let modified = metadoc.date(forKey: "modified")
    
    print("    format: ", format ?? "<missing>")
    print("   created: ", created ?? "<missing>")
    print("  modified: ", modified ?? "<missing>")
}


// MARK: -
// MARK: store operations

func getStoreURL () -> URL? {
    if let appSupportDir = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first {
        return appSupportDir.appendingPathComponent(kDelectusStoreDirectoryName, isDirectory: true)
    } else {
        print("Failed to locate the Application Support directory")
        return nil
    }
}

func getStoreMetadata (db: Database) -> Document? {
    return db.document(withID: kDelectusStoreMetadataID)
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
    if (delectusStore != nil) {
        return delectusStore
    } else {
        if  let dataDir = findOrCreateStoreDirectory() {
            let dataPath = dataDir.path
            let conf = DatabaseConfiguration()
            conf.directory = dataPath
            do {
                let db = try Database(name: kDelectusStoreDBName, config: conf)
                if let metadoc = getStoreMetadata(db: db) {
                    print("Delectus store:")
                    printStoreMetadata(metadoc: metadoc)
                } else {
                    let metadoc = makeStoreMetadataDocument()
                    print("creating new metadata document...")
                    print("Delectus store:")
                    printStoreMetadata(metadoc: metadoc)
                    try db.saveDocument(metadoc)
                }
                delectusStore = db
                return db
            } catch {
                fatalError("Can't open the Delectus store")
            }
        } else {
            fatalError("Can't locate the Delectus store")
        }
    }
}

func closeStore () {
    if let store = delectusStore {
        do { try store.close() }
        catch { print("Unable to close the Delectus store") }
    }
}


// MARK: -
// MARK: lists


