import React, { Component } from 'react';
import './App.css';
import Button from '@material-ui/core/Button';

// ---------------------------------------------------------
// EditorContentsBox component
// ---------------------------------------------------------
// Presents the contents of a doc for editing

class EditorContentsBox extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
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

    return (
        <table className="EditorButtonBarTable">
            <tbody>
                <tr>
                    <td>
                        <Button
                            variant="contained"
                            color="primary"
                            onClick={app.cancelAndDismissDocumentEditor}>
                            Save
                        </Button>
                    </td>
                    <td>
                        <Button
                            variant="contained"
                            color="primary"
                            onClick={app.cancelAndDismissDocumentEditor}>
                            Cancel
                        </Button>
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
        const app = this.props.app;
        const doc = this.props.doc;

        return (
            <div className="Frame" id={doc._id}>
                <h1>Opps Daily</h1>
                <p>Editing document: {doc._id}</p>
                <EditorContentsBox app={app} doc={doc} />
                <EditorButtonBar app={app} doc={doc} editor={this} />
            </div>
        );
    }

}

export default DocEditor;
