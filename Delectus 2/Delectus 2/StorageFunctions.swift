//
//  DxStore.swift
//  Delectus 2
//
//  Created by mikel evins on 10/2/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation

func collectionPathToName (url: URL) -> String {
    if (url.isFileURL) {
        return url.deletingPathExtension().lastPathComponent
    } else {
        let msg = "Not a file url: \(url.absoluteString)"
        fatalError(msg)
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
    if let url = storeDataDirectory() {
        do {
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

func storeDataDirectory() -> URL? {
    return FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first
}
