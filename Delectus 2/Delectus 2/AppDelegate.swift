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
        // for legacy file conversions
        initDelectus1Engine()
        // make sure the store's local DB closes on termination
        registerTerminationObserver()
        // print the store to confirm that it initialized properly
        print("\n\(store)")
    }
    
    func applicationWillTerminate(_ aNotification: Notification) {
        print("\nApplication about to terminate")
        store.close()
        deinit_delectus1_engine()
    }
    
    func initDelectus1Engine() {
        // initialize the Delectus 1 engine for file conversions
        // TODO: add error checking
        init_delectus1_engine()
        print("Delectus 1 engine initialized")
    }
    
    func registerTerminationObserver() {
        // works only if the NSSupportsSuddenTermination key in Info.plist has the value NO
        let nc = NotificationCenter.default
        nc.addObserver(self, selector: #selector(applicationWillTerminate), name: Notification.Name("ApplicationWillTerminate"), object: nil)
    }
    
}

