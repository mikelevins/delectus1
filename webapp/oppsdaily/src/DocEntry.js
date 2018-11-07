import React, { Component } from 'react';
import './App.css';
import DocCompactField from './DocCompactField.js';

// ---------------------------------------------------------
// DocEntry component
// ---------------------------------------------------------
// Displays an individual document entry

class DocEntry extends Component {
    render() {
        var entry = this.props.entry;
        var entry_id = entry.id;

        return (
            <div className="DocEntry">
                <p className="left">id: {entry_id}</p>
            </div>
        );
    }
}

export default DocEntry;
