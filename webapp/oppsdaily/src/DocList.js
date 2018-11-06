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
        <div className="Table">
            {docEntries}
        </div>
    );

}

export default DocList;
