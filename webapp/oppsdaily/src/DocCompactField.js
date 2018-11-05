import React from 'react';
import './App.css';

// ---------------------------------------------------------
// DocCompactField component
// ---------------------------------------------------------

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
