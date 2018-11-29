import React, { Component } from 'react';
import './App.css';
import Button from '@material-ui/core/Button';

const styles = {
    browserPane: {
        border: '1px solid black',
        height: '24rem',
        overflow: 'auto',
        padding: '6px',
        width: '32rem'
    },
    browserPaneTitle: {
        fontWeight: 'bold',
        height: '1rem',
    }
};


class BrowserDocumentContentsPane extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const app = pane.props.app;
        const docID = pane.props.documentID;
        const paneTitle = docID;
        const doc = app.getSelectedDocument();
        const docKeys = (doc) ? Object.keys(doc) : [];
        const docRows = docKeys.map((key) => {
            const val = doc[key];
            return (
                <tr><td>{key}</td><td>{val}</td></tr>
            );
        });

        if (doc) {
            return (
                <div>
                    <p style={styles.browserPaneTitle}>{paneTitle}</p>
                    <div style={styles.browserPane}>
                        <table>
                            <tbody>
                                {docRows}
                            </tbody>
                        </table>
                    </div>
                </div>
            );
        } else {
            return (
                <div>
                    <p style={styles.browserPaneTitle}>{paneTitle}</p>
                </div>
            );
        }



    }
}

// exports
// ---------------------------------------------------------

export default BrowserDocumentContentsPane;

