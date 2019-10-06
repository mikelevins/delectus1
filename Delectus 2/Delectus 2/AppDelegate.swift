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
    var store = Store()
    
    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // used to close open databases
        // works only if the NSSupportsSuddenTermination key in Info.plist has the value NO
        let nc = NotificationCenter.default
        nc.addObserver(self, selector: #selector(applicationWillTerminate), name: Notification.Name("ApplicationWillTerminate"), object: nil)
        // initialize the Delectus 1 engine for file conversions
        init_delectus1_engine()
        print("\n\(store)")
    }
    
    func applicationWillTerminate(_ aNotification: Notification) {
        print("\nApplication about to terminate")
        //closeStore()
        //print("Closed the Delectus store")
    }
    
}

