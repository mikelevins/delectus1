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
    // TODO: Open or create the default Collection (i.e. CBL db) at app startup

    @IBOutlet weak var window: NSWindow!

    // MARK: - Properties
    // ----------------------------------------------------------------

    // the macOS-defined container for application data
    var dataDirectory: URL? {
        get { appDataDirectory() }
    }
    
    // MARK: - Methods

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        init_delectus1_engine()
        print("Application data directory: ", self.dataDirectory ?? "<none>")
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }

}

func appDataDirectory () -> URL? {
    return FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first
}

