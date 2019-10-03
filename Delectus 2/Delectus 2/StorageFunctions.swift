//
//  DxStore.swift
//  Delectus 2
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation

let DelectusStoreName = "com.mikelevins.delectus.Store"

func collectionURLToName (url: URL) -> String {
    if (url.isFileURL) {
        return url.deletingPathExtension().lastPathComponent
    } else {
        let msg = "Not a file url: \(url.absoluteString)"
        fatalError(msg)
    }
}

func getDataDirectory() -> URL? {
    let urls = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)
    let appSupportDir = urls[0]
    let dataPath = appSupportDir.appendingPathComponent(DelectusStoreName)
    print("dataPath = ",dataPath.path)
    if (FileManager.default.fileExists(atPath: dataPath.path)) {
        print("dataPath exists!")
        return dataPath
    } else {
        print("dataPath does not exist")
        do {
            try FileManager.default.createDirectory(atPath: dataPath.path, withIntermediateDirectories: true, attributes: nil)
            print("dataPath created")
            return dataPath
        } catch {
            print("Failed to create dataPath")
            print(error.localizedDescription);
            return nil
        }
    }
}

func isDelectusCollection (url: URL) -> Bool {
    let isFile = url.isFileURL
    let isCBLite = url.path.hasSuffix("cblite2")
    if (isFile && isCBLite) {
        return true
    } else {
        return false
    }
}

func knownCollections () -> [URL] {
    let mgr = FileManager.default
    if let url = getDataDirectory() {
        do {
            print("Data directory = ", url.path)
            let paths = try mgr.contentsOfDirectory(at: url, includingPropertiesForKeys: nil, options: FileManager.DirectoryEnumerationOptions.skipsSubdirectoryDescendants)
            let result = paths.filter({ isDelectusCollection(url: $0)} )
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
