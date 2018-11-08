import React, { Component } from 'react';
import './App.css';

// ---------------------------------------------------------
// EditorContentsBox component
// ---------------------------------------------------------
// Presents the contents of a doc for editing

class EditorContentsBox extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        var app = this.props.app;
        var doc = this.props.doc;
        var doc_keys = Object.keys(doc).reverse();
        var field_rows = doc_keys.map(
            (key) => {
                return (
                    <tr key={key}>
                        <td className="bold right Cell">{key}:</td>
                        <td className="left Cell">
                            <textarea
                                name={key}
                                className="EditorTextArea"
                                defaultValue={doc[key]} />
                        </td>
                    </tr>
                );
            }
        );

        return (
            <table className="EditorContentsTable">
                <tbody>
                    {field_rows}
                </tbody>
            </table>
        );
    }

}

// ---------------------------------------------------------
// EditorButtonBar component
// ---------------------------------------------------------
// Displays action buttons for the containing editor

function EditorButtonBar(props) {
    var app = props.app;
    var editor = props.editor;

    return (
        <table>
            <tbody>
                <tr>
                    <td>
                        <button
                            className="Button"
                            onClick={() => { app.cancelAndDismissDocumentEditor() }}>
                            Save</button>
                    </td>
                    <td>
                        <button
                            className="Button"
                            onClick={() => { app.cancelAndDismissDocumentEditor() }}>
                            Cancel</button>
                    </td>
                </tr>
            </tbody>
        </table>
    );

}

// ---------------------------------------------------------
// DocEditor component
// ---------------------------------------------------------
// Presents an editor dialog for document entries

class DocEditor extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        var app = this.props.app;
        var doc = this.props.doc;

        return (
            <div className="Frame">
                <h1>Opps Daily</h1>
                <p>Editing document: {doc._id}</p>
                <EditorContentsBox app={app} doc={doc} />
                <EditorButtonBar app={app} editor={this} />
            </div>
        );
    }

}

export default DocEditor;
