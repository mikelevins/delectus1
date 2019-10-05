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
// MARK: class Store

class Store : CustomStringConvertible{
    var pathURL = findOrCreateStoreDirectory()
    lazy var database = openCBLDatabase(pathURL)
    
    var metadata: Document? { get { return database.document(withID: kDelectusStoreMetadataID) } }
    
    var description: String {
        let path = pathURL.path
        let name = database.name
        return "Store:\n  name: \(name)\n  path: \(path)"
    }
}

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
    
    print("\nStore metadata:")
    print("    format: ", format ?? "<missing>")
    print("   created: ", created ?? "<missing>")
    print("  modified: ", modified ?? "<missing>")
}

// MARK: -
// MARK: store operations

func getStoreURL () -> URL? {
    if let suppURL = applicationSupportURL() {
        return suppURL.appendingPathComponent(kDelectusStoreDirectoryName, isDirectory: true)
    } else {
        return nil
    }
}

func findOrCreateStoreDirectory() -> URL {
    if let storeURL = getStoreURL() {
        if (urlPathExists(storeURL)) {
            print("\nStore directory exists; returning it...\n")
            return storeURL
        } else {
            print("\nStore directory does not exist; trying to create it...\n")
            if let result = createURLPath(storeURL) {
                return result
            } else {
                fatalError("Can't create the Store directotry at \(storeURL.path)")
            }
        }
    } else {
        fatalError("\nFailed to get the store path")
    }
}

func openCBLDatabase(_ url:URL) -> Database {
    let dataPath = url.path
    let conf = DatabaseConfiguration()
    conf.directory = dataPath
    do {
        let db = try Database(name: kDelectusStoreDBName, config: conf)
        if (db.document(withID: kDelectusStoreMetadataID) != nil) {
            return db
        } else {
            let metadoc = makeStoreMetadataDocument()
            print("\ncreating new metadata document...")
            print("Delectus store:")
            try db.saveDocument(metadoc)
            return db
        }
    } catch {
        fatalError("Can't open the Delectus store")
    }
}

//func closeStore () {
//    if let store = delectusStore {
//        do { try store.close() }
//        catch { print("Unable to close the Delectus store") }
//    }
//}


// MARK: -
// MARK: lists


