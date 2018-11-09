import React, { Component } from 'react';
import './App.css';
import List from '@material-ui/core/List';
import DocEntry from './DocEntry.js';

// ---------------------------------------------------------
// DocList component
// ---------------------------------------------------------
// Displays the list of document entries

class DocList extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const app = this.props.app;
        const docs = this.props.docs;
        const entries = docs.map((doc) =>
            <DocEntry key={doc.id} app={app} entry={doc} />);

        return (
            <List>
                <h1>Opps Daily</h1>
                <h4>Document count: {docs.length}</h4>
                {entries}
            </List>
        );
    }

} // end DocList


// ---------------------------------------------------------
// exports
// ---------------------------------------------------------

export default DocList;
