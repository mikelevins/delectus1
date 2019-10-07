//
//  AppDelegate.swift
//  Delectus 2
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Cocoa

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {
    var store = Store()
    
    func applicationDidFinishLaunching(_ aNotification: Notification) {
        // initialize the Delectus 1 engine for file conversions
        initDelectus1Engine()
        // make sure the store's local DB closes on termination
        registerTerminationObserver()
        // print the store to confirm that it initialized properly
        print("\n\(store)")
    }
    
    func applicationWillTerminate(_ aNotification: Notification) {
        print("\nApplication about to terminate")
        closeStore(store)
        deinit_delectus1()
    }
    
    func initDelectus1Engine()  {
        let status = init_delectus1();
        if (status == ERR_NO_ERROR) {
            print("Delectus 1 compatibility engine initialized")
        } else {
            fatalError("Can't initialize the Delectus 1 compatibility engine")
        }
    }
    
    func registerTerminationObserver() {
        // watches for impending app termination
        // works only if the NSSupportsSuddenTermination key in Info.plist has the value NO
        let nc = NotificationCenter.default
        nc.addObserver(self, selector: #selector(applicationWillTerminate), name: Notification.Name("ApplicationWillTerminate"), object: nil)
    }
    
}

