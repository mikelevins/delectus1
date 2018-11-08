import React, { Component } from 'react';
import './App.css';
import EditorField from './EditorField.js';
import EditorButtonBar from './EditorButtonBar.js';

// ---------------------------------------------------------
// DocEditor component
// ---------------------------------------------------------
// Presents an editor dialog for document entries

class DocEditor extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        var app = this.props.app;
        var doc = this.props.doc
        return (
            <div className="DocEditor">
                <h1>Opps Daily</h1>
                <p>Editing document: {doc._id}</p>
                <EditorButtonBar app={app} editor={this} />
            </div>
        );
    }

}

export default DocEditor;
