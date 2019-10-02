//
//  AppDelegate.swift
//  Delectus 2
//
//  Created by mikel evins on 9/23/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Cocoa
import CouchbaseLiteSwift

// MARK: - Class AppDelegate

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {
    // TODO: Open or create the default Collection (i.e. CBL db) at app startup

    @IBOutlet weak var window: NSWindow!

    // MARK: - Properties
    // ----------------------------------------------------------------
    
    func applicationDidFinishLaunching(_ aNotification: Notification) {
        init_delectus1_engine()
        
        let db = openOrCreateDefaultCollectionDB();
        if let meta = db.document(withID: CollectionMetadataID) {
            print("\nFound the default collection DB with metadata: ",meta.toDictionary())
        } else {
            print("\nThe default collection DB metadata was not found")
        }
        
        //let collections = knownCollectionNames()
        //print("Known collections = ", collections)
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }

}

// MARK: - Static functions


