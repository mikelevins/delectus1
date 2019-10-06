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
        var metadescription: String
        
        if let meta = metadata {
            metadescription = describeStoreMetadata(metadoc: meta)
        } else {
            metadescription = "\n<missing metadata>\n"
        }
        
        let result = """
        Store:\n  name: \(name)\n  path: \(path)
        \(metadescription)
        """
        return result
    }
}

// MARK: -
// MARK: store metadata

func makeStoreMetadataDocument () -> MutableDocument {
    let metadoc = MutableDocument(id: kDelectusStoreMetadataID)
        .setString(kTypeMetadata, forKey: kKeyType)
        .setString(kDelectusStoreFormatVersion, forKey: kMetadataKeyFormatVersion)
        .setDate(Date(), forKey: kMetadataKeyCreated)
        .setDate(Date(), forKey: kMetadataKeyModified)
    return metadoc
}

func describeStoreMetadata (metadoc: Document) ->String {
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


