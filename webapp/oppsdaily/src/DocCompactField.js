import React from 'react';
import './App.css';

// ---------------------------------------------------------
// DocCompactField component
// ---------------------------------------------------------
// uses app.formatForDisplay() to shorten very long fields for display
// BUG: assumes the very long field is the content field

function DocCompactField(props) {
    var app = props.app;

    return (
        <div className="Row">
            <div className="Label">{
                props.docKey}:
          </div>
            <div className="Cell">
                {app.formatForDisplay({ value: props.doc[props.docKey] })}
            </div>
        </div>
    );
}

export default DocCompactField;
