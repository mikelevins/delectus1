
import React from 'react';
import './App.css';

// ---------------------------------------------------------
// EditorButtonBar component
// ---------------------------------------------------------
// Displays action buttons for the containing editor

function EditorButtonBar(props) {
    var app = props.app;
    var editor = props.editor;

    return (
        <div className="Table">
            <div className="Row">
            <div className="Cell">
                    <button
                        className="Button f12pt"
                        onClick={() => { app.cancelAndDismissDocumentEditor() }}>
                        Save</button>
                </div>
                <div className="Cell">
                    <button
                        className="Button f12pt"
                        onClick={() => { app.cancelAndDismissDocumentEditor() }}>
                        Cancel</button>
                </div>
            </div>
        </div>
    );

}

export default EditorButtonBar;
