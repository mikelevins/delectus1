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

class Store : CustomStringConvertible {
    var pathURL: URL  { findOrCreateStoreDirectory() }
    var metadata: Document? { return database.document(withID: kDelectusStoreMetadataID) }
    var description: String { return describeStore() }
    lazy var database = openStoreDatabase()

    func describeStore () ->String {
        let path = pathURL.path
        let name = database.name
        let metadescription = describeStoreMetadata()
        
        let result = """
        Store:\n  name: \(name)\n  path: \(path)
        \(metadescription ?? "<metadata missing>")
        """
        return result
    }
    
    func openStoreDatabase() -> Database {
        let conf = DatabaseConfiguration()
        conf.directory = pathURL.path
        var db: Database
        
        // open the database
        do { db = try Database(name: kDelectusStoreDBName, config: conf) }
        catch { fatalError("Can't open the Delectus store") }
        
        // check to make sure the opened db has a metadata document
        if (db.document(withID: kDelectusStoreMetadataID) == nil) {
            // no metadata found; create and save it
            print("\ncreating new metadata document...")
            let metadoc = makeStoreMetadataDocument()
            
            do { try db.saveDocument(metadoc) }
            catch { fatalError("\nCan't save the store's metadata") }
        }
        
        return db
    }
    
    func describeStoreMetadata () -> String? {
        if let metadoc = metadata {
            let doctype = metadoc.string(forKey: kKeyType)
            let format = metadoc.string(forKey: kMetadataKeyFormatVersion)
            let created = metadoc.date(forKey: kMetadataKeyCreated)
            let createdString = (created != nil) ? String(describing: created) : "<missing>"
            let modified = metadoc.date(forKey: kMetadataKeyModified)
            let modifiedString = (modified != nil) ? String(describing: modified) : "<missing>"
                        
            let result = """
            Store metadata:
            type: \(doctype ?? "<missing>")
            format: \(format ?? "<missing>")
            created: \(createdString)
            modified: \(modifiedString)
            """
            return result
        } else {
            return nil
        }
        
    }
    
    func close () {
        do {
            try database.close()
            print("Closed the Delectus store")
        }
        catch { print("Unable to close the Delectus store") }
    }
}

// MARK: -
// MARK: store metadata

func makeStoreMetadataDocument () -> MutableDocument {
    let metadoc = MutableDocument(id: kDelectusStoreMetadataID)
        .setString(kTypeStoreMetadata, forKey: kKeyType)
        .setString(kDelectusStoreFormatVersion, forKey: kMetadataKeyFormatVersion)
        .setDate(Date(), forKey: kMetadataKeyCreated)
        .setDate(Date(), forKey: kMetadataKeyModified)
    return metadoc
}


// MARK: -
// MARK: store auxiliary operations

func findOrCreateStoreDirectory() -> URL {
    if let storeURL = applicationSupportURL()?.appendingPathComponent(kDelectusStoreDirectoryName, isDirectory: true) {
        if (FileManager.default.fileExists(atPath: storeURL.path)) {
            return storeURL
        } else {
            print("\nStore directory does not exist; trying to create it...\n")
            do {
                try FileManager.default.createDirectory(atPath: storeURL.path, withIntermediateDirectories: true, attributes: nil)
                return storeURL
            } catch {
                fatalError("Failed to create file or directory at path \(storeURL.path)")
            }
        }
    } else {
        fatalError("\nFailed to get the store path")
    }
}




// MARK: -
// MARK: lists


