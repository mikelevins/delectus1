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
        
        var metadescription: String
        if let metadoc = metadata {
            metadescription = describeStoreMetadata(metadoc)
        } else {
            metadescription = "<metadata missing>"
        }
        
        let result = """
        Store:\n  name: \(name)\n  path: \(path)
        \(metadescription)
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

    
    func close () {
        do {
            try database.close()
            print("Closed the Delectus store")
        }
        catch { print("Unable to close the Delectus store") }
    }
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


