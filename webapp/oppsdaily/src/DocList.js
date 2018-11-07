import React, { Component } from 'react';
import './App.css';
import DocEntry from './DocEntry.js';

// ---------------------------------------------------------
// DocList component
// ---------------------------------------------------------
// Displays the list of document entries

class DocList extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const docs = this.props.docs;
        const entries = docs.map(
            (doc) => <p>id: {doc.id}</p>
        );

        return (
            <div className="DocList">
            {entries}}
            </div>
        );
    } // end render
} // end DocList


// ---------------------------------------------------------
// exports
// ---------------------------------------------------------

export default DocList;
