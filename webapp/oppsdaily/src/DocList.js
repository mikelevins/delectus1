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
        const entries = docs.map( (doc) => <DocEntry key={doc.id} entry={doc} /> );

        return (
            <div className="DocList">
                <h1>Opps Daily</h1>
                <h4>Document count: {docs.length}</h4>
                {entries}
            </div>
        );
    } // end render
} // end DocList


// ---------------------------------------------------------
// exports
// ---------------------------------------------------------

export default DocList;
