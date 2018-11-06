
import React from 'react';
import './App.css';

// ---------------------------------------------------------
// EditorField component
// ---------------------------------------------------------
// Displays CouchDB document fields for editing
// BUG: special-cases the 'content' field

const readOnlyFieldNames = ['_rev', '_id', 'date_received', 'message_id'];

function EditorField(props) {
    var labelText = props.docKey;
    var valueText = props.doc[props.docKey];

    if (readOnlyFieldNames.includes(props.docKey)) {
        return (
            <div className="Row">
                <div className="Label">
                    <label>{labelText}:</label>
                </div>
                <div className="Cell">
                    <span>{valueText}</span>
                </div>
            </div>
        );
    } else if (props.docKey === "content") {
        return (
            <div className="Row">
                <div className="Label">
                    <label>{labelText}:</label>
                </div>
                <div className="Cell">
                    <textarea
                        className="TextAreaCell"
                        name={props.docKey}
                        defaultValue={valueText} />
                </div>
            </div>);
    } else {
        return (
            <div className="Row">
                <div className="Label">
                    <label>{labelText}:</label>
                </div>
                <div className="Cell">
                    <input className="InputCell"
                        name={props.docKey}
                        type="text" defaultValue={valueText} />
                </div>
            </div>
        );
    }
}

export default EditorField;
