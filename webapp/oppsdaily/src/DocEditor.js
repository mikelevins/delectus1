import React from 'react';
import './App.css';
import EditorField from './EditorField.js';
import EditorButtonBar from './EditorButtonBar.js';

// ---------------------------------------------------------
// DocEditor component
// ---------------------------------------------------------
// Presents an editor dialog for document entries

function DocEditor(props) {
    var app = props.app;
    var doc = props.doc
    var doc_keys = Object.keys(doc).reverse();
    var doc_fields = doc_keys.map((k) =>
        <EditorField
            key={k}
            docKey={k}
            doc={doc} />);

    return (
        <div>
            <h1 className="center">Opps Daily</h1>
            <h3 className="center"> Editing document id: {app.state.selectedDoc._id}</h3>
            <div className="Editor">
                <div className="Table">
                    {doc_fields}
                </div>
                <EditorButtonBar app={app} editor={this} />
            </div>
        </div>
    );
}

export default DocEditor;
