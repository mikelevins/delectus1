import React from 'react';
import './App.css';
import EditorField from './EditorField.js';

// ---------------------------------------------------------
// DocEditor component
// ---------------------------------------------------------

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
      <div className="Editor">
        {doc_fields}
        <button className="Button" onClick={() => { app.hideEditor() }}>Close</button>
      </div>
    );
  }
  
  export default DocEditor;
