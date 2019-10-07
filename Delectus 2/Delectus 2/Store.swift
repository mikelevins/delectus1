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
    var pathURL = findOrCreateStoreDirectory()
    lazy var database = openStoreDatabase()
    var metadata: Document? { get { return database.document(withID: kDelectusStoreMetadataID) } }
    var description: String { return describeStore() }
    
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
        let dataPath = pathURL.path
        let conf = DatabaseConfiguration()
        conf.directory = dataPath
        var db: Database
        
        // open the database
        do {
            db = try Database(name: kDelectusStoreDBName, config: conf)
        } catch {
            fatalError("Can't open the Delectus store")
        }
        
        // check to make sure the opened db has a metadata document
        let metadoc = db.document(withID: kDelectusStoreMetadataID)
        if (metadoc == nil) {
            // no metadata found; create and save it
            print("\ncreating new metadata document...")
            let new_metadoc = makeStoreMetadataDocument()
            do {
                try db.saveDocument(new_metadoc)
                return db
            } catch {
                fatalError("Can't save the store's metadata")
            }
        } else {
            // we got a good metadata document; return the database
            return db
        }
    }
    
    func describeStoreMetadata () -> String? {
        if let metadoc = metadata {
            let doctype = metadoc.string(forKey: kKeyType)
            let format = metadoc.string(forKey: kMetadataKeyFormatVersion)
            let created = metadoc.date(forKey: kMetadataKeyCreated)
            var createdString: String
            let modified = metadoc.date(forKey: kMetadataKeyModified)
            var modifiedString: String
            
            if let created = created { createdString = String(describing: created) } else { createdString = "<missing>" }
            if let modified = modified { modifiedString = String(describing: modified) } else { modifiedString = "<missing>" }
            
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
        if (urlPathExists(storeURL)) {
            return storeURL
        } else {
            print("\nStore directory does not exist; trying to create it...\n")
            if let result = createURLPath(storeURL) {
                return result
            } else {
                fatalError("Can't create the Store directory at \(storeURL.path)")
            }
        }
    } else {
        fatalError("\nFailed to get the store path")
    }
}




// MARK: -
// MARK: lists


