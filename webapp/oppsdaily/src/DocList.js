import React from 'react';
import './App.css';
import DocEntry from './DocEntry.js';

// ---------------------------------------------------------
// DocList component
// ---------------------------------------------------------
// Displays the list of document entries

function DocList(props) {
    const entries = props.documents;
    const docEntries = entries.map(
        (entry) =>
            <DocEntry
                key={entry.key}
                entry={entry}
                app={props.app}
            />);

    return (
        <div>
            <h1 className="center">Opps Daily</h1>
            <h3 className="center">document count: {props.app.state.allDocs.length}</h3>

            <div className="Table">
                {docEntries}
            </div>
        </div>
    );

}

export default DocList;
