//
//  AppDelegate.swift
//  Delectus 2
//
//  Created by mikel evins on 9/23/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Cocoa
import CouchbaseLiteSwift

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {

    @IBOutlet weak var window: NSWindow!

    // MARK: - Properties
    // ----------------------------------------------------------------

    // the macOS-defined container for application data
    var dataDirectory: URL? {
        get { appDataDirectory() }
    }
    
    let couchBase: Database

    // MARK: - Initializers
    override init(){
        if let dataDir = appDataDirectory()
        {
            let conf = DatabaseConfiguration()
            conf.directory = dataDir.path;
            var db: Database
            do {
                db = try Database(name: "DelectusDB", config: conf)
                self.couchBase = db
            } catch {
                fatalError("Unable to connect to CouchBase")
            }
        } else {
            fatalError("No data directory found.")
        }

    }
    
    // MARK: - Methods

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        init_delectus1_engine()
        print("Application data directory: ", self.dataDirectory)
        print("The CouchBase instance is: ", self.couchBase.name)
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }

}

func appDataDirectory () -> URL? {
    return FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first
}

