//
//  AppDelegate.swift
//  Delectus 2
//
//  Created by mikel evins on 9/23/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Cocoa
import CouchbaseLiteSwift

let DefaultCollectionName = "DefaultCollection"
let CollectionMetadataID = "list_metadata"

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {
    // TODO: Open or create the default Collection (i.e. CBL db) at app startup

    @IBOutlet weak var window: NSWindow!

    // MARK: - Properties
    // ----------------------------------------------------------------

    // the macOS-defined container for application data
    var dataDirectory: URL? {
        get { appDataDirectory() }
    }
    
    var defaultCollectionDB: Database
    
    // MARK: - Methods
    override init() {
        defaultCollectionDB = openOrCreateDefaultCollectionDB()
        super.init()
    }
    
    func applicationDidFinishLaunching(_ aNotification: Notification) {
        init_delectus1_engine()
        print("Application data directory: ", self.dataDirectory ?? "<none>")
        print("Default collection database: ", self.defaultCollectionDB)
        
        if let meta = defaultCollectionDB.document(withID: CollectionMetadataID) {
            print("Found the default collection DB with metadata ",meta.toDictionary())
        } else {
            print("No default collection DB found ")
        }
        
        let collectionPaths = knownCollections()
        let collectionURLs = collectionPaths.map({ URL(fileURLWithPath: $0) })
        let collections = collectionURLs.map({ DxCollection(withPathURL: $0) })
        let foundPaths = collections.map({ $0.pathURL })
        print("Known collections = ", foundPaths)
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }

}

func appDataDirectory () -> URL? {
    return FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first
}

func knownCollections () -> [String] {
    let mgr = FileManager.default
    if let url = appDataDirectory() {
        let path = url.path
        do {
            let paths = try mgr.contentsOfDirectory(atPath: path)
            let result = paths.filter({ $0.hasSuffix(".cblite2")})
            return result
        } catch {
            print("Unable to get the contents of the app data directory")
            return []
        }
    } else {
        print("Unable to locate the app data directory")
        return []
    }
}


func openOrCreateDefaultCollectionDB () -> Database {
    if  let dataDir = appDataDirectory() {
        let dataPath = dataDir.path
        let conf = DatabaseConfiguration()
        conf.directory = dataPath
        do {
            let db = try Database(name: DefaultCollectionName, config: conf)
            print("created the default collection DB")
            if let metadoc = db.document(withID: CollectionMetadataID) {
                print("found default collection metadata: ", metadoc)
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
            fatalError("Can't open or create the default collection database")
        }
    } else {
        fatalError("Can't locate the application data directory")
    }
}

