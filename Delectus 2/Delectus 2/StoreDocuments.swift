//
//  StoreDocuments.swift
//  Delectus 2
//
//  Created by mikel evins on 10/7/19.
//  Copyright © 2019 mikel evins. All rights reserved.
//

import Foundation
import CouchbaseLiteSwift

// MARK: -
// MARK: store metadata

func makeStoreMetadataDocument () -> MutableDocument {
    let metadoc = MutableDocument(id: kDelectusStoreMetadataID)
        .setString(kTypeStoreMetadata, forKey: kKeyType)
        .setString(kDelectusStoreFormatVersion, forKey: kMetadataKeyFormatVersion)
        .setDate(Date(), forKey: kMetadataKeyCreated)
        .setDate(Date(), forKey: kMetadataKeyModified)
    return metadoc
}


func describeStoreMetadata (_ metadoc: Document) -> String {
    let doctype = metadoc.string(forKey: kKeyType)
    let format = metadoc.string(forKey: kMetadataKeyFormatVersion)
    let created = metadoc.date(forKey: kMetadataKeyCreated)
    let createdString = (created != nil) ? String(describing: created) : "<missing>"
    let modified = metadoc.date(forKey: kMetadataKeyModified)
    let modifiedString = (modified != nil) ? String(describing: modified) : "<missing>"
                
    let result = """
    Store metadata:
    type: \(doctype ?? "<missing>")
    format: \(format ?? "<missing>")
    created: \(createdString)
    modified: \(modifiedString)
    """
    return result
}
