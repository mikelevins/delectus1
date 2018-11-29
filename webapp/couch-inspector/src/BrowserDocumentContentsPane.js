import React, { Component } from 'react';
import './App.css';
import Button from '@material-ui/core/Button';

const styles = {
    browserPane: {
        border: '1px solid black',
        height: '20rem',
        overflow: 'auto',
        padding: '6px',
        width: '100%',
    },
    browserPaneTitle: {
        fontWeight: 'bold',
        height: '1rem',
    },
    button: {
        textTransform: 'none'
    }
};


class BrowserDocumentContentsPane extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const app = pane.props.app;
        const docID = pane.props.documentID;
        const doc = app.getSelectedDocument();
        const docStr = (doc) ? JSON.stringify(doc) : "" ;
        const paneTitle = docID;

        return (
            <div>
                <p style={styles.browserPaneTitle}>{paneTitle}</p>
                <p style={styles.documentContentsPane}>{docStr}</p>
            </div>
        );


    }
}

// exports
// ---------------------------------------------------------

export default BrowserDocumentContentsPane;

