//
//  AppDelegate.swift
//  Delectus 2
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Cocoa
import CouchbaseLiteSwift

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {
    
    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // used to close open databases
        // works only if the NSSupportsSuddenTermination key in Info.plist has the value NO
        let nc = NotificationCenter.default
        nc.addObserver(self, selector: #selector(applicationWillTerminate), name: Notification.Name("ApplicationWillTerminate"), object: nil)
        init_delectus1_engine()
        if let storeDB = openStore() {
            let storePath = storeDB.path ?? "<absent>"
            print("\nThe local Delectus store is:\n  \(storePath)")
        } else {
            fatalError("\nUnable to locate the Delectus store")
        }
    }
    
    func applicationWillTerminate(_ aNotification: Notification) {
        print("\nApplication about to terminate")
        closeStore()
        print("Closed the Delectus store")
    }
    
}

