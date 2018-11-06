import React from 'react';
import './App.css';
import DocCompactField from './DocCompactField.js';

// ---------------------------------------------------------
// DocEntry component
// ---------------------------------------------------------
// Displays an individual document entry

function DocEntry(props) {
    var app = props.app;
    var entry = props.entry;
    var entry_doc = entry.doc;
    var doc_keys = Object.keys(entry_doc).reverse();
    var doc_fields = doc_keys.map((k) =>
        <DocCompactField
            app={app}
            key={k}
            docKey={k}
            doc={entry_doc} />);

    return (
        <div className="Entry">
            {doc_fields}
            <div className="Row">
                <div className="EntryField">
                    <button className="Button f12pt" onClick={() => {
                        app.editDocument({ app: app, doc: entry_doc })
                    }
                    }>
                        Edit
            </button>
                </div>
            </div>
        </div>
    );
}

export default DocEntry;
