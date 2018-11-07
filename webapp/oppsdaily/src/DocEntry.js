import React, { Component } from 'react';
import './App.css';
import DocCompactField from './DocCompactField.js';

// ---------------------------------------------------------
// DocEntry component
// ---------------------------------------------------------
// Displays an individual document entry

class DocEntry extends Component {
    render() {
        const entry = this.props.entry;
        const entry_id = entry.id;
        const doc = entry.doc;

        return (
            <div className="DocEntry">
                <p className="left">
                    <span className='bold'>date:</span> {doc.date_received}, &nbsp;
                    </p>
                <p className='left'>
                    <span className='bold'>id:</span> {entry_id}
                </p>
            </div >
        );
    }
}

export default DocEntry;
