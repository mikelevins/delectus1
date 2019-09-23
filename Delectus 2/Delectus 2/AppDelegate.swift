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
    
    var dataDirectory: URL? {
        get {
            let defaultDataDirs = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)
            if let dir = defaultDataDirs.first {
                return dir
            } else {
                return nil
            }
        }
    }

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // initialize the Delectus Scheme engine
        init_engine()
        print("Application data directory is ", self.dataDirectory ?? "nil")
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
    }


}

