//
//  DxStore.swift
//  Delectus 2: Interface to local storage
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation
import CouchbaseLiteSwift

// MARK: - Constants

let DefaultCollectionName = "DefaultCollection"
let CollectionMetadataID = "list_metadata"

// MARK: - Static functions

func fileBaseName (url: URL) -> String {
    if (url.isFileURL) {
        return url.deletingPathExtension().lastPathComponent
    } else {
        let msg = "Not a file url: \(url.absoluteString)"
        fatalError(msg)
    }
}

func knownCollectionNames () -> [String] {
    let paths = knownCollectionPaths()
    let urls = paths.map({ URL(fileURLWithPath: $0) })
    return urls.map({ fileBaseName(url: $0) })
}

func knownCollectionPaths () -> [String] {
    let mgr = FileManager.default
    if let url = storeDataDirectory() {
        let path = url.path
        do {
            let paths = try mgr.contentsOfDirectory(atPath: path)
            let result = paths.filter({ $0.hasSuffix(".cblite2")})
            return result
        } catch {
            print("\nUnable to get the contents of the app data directory")
            return []
        }
    } else {
        print("\nUnable to locate the app data directory")
        return []
    }
}

func openOrCreateDefaultCollectionDB () -> Database {
    if  let dataDir = storeDataDirectory() {
        let dataPath = dataDir.path
        let conf = DatabaseConfiguration()
        conf.directory = dataPath
        do {
            let db = try Database(name: DefaultCollectionName, config: conf)
            print("\nfound or created the default collection DB")
            
            if let metadoc = db.document(withID: CollectionMetadataID) {
                print("\nfound default collection metadata: ", metadoc)
                return db
            } else {
                let new_metadoc = MutableDocument(id: CollectionMetadataID)
                    .setString("delectus_collection", forKey: "type")
                    .setDate(Date(), forKey: "created")
                    .setDate(Date(), forKey: "modified")
                    .setBoolean(false, forKey: "deleted")
                try db.saveDocument(new_metadoc)
                return db
            }
        } catch {
            fatalError("\nCan't open or create the default collection database")
        }
    } else {
        fatalError("\nCan't locate the application data directory")
    }
}

func storeDataDirectory() -> URL? {
    return FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first
}

