import React, { Component } from 'react';
import './App.css';
import BrowserDatabasesPane from './BrowserDatabasesPane.js';
import BrowserDocumentsPane from './BrowserDocumentsPane.js';
import BrowserDocumentContentsPane from './BrowserDocumentContentsPane.js';

const styles = {
    browser: {
        borderSpacing: '8px 8px',
    },
    browserPane: {
        border: '1px solid black',
        padding: '10px 5px',
        verticalAlign: 'top',
    },
};

class Browser extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const browser = this;
        const app = browser.props.app;
        const middlePaneList = [];
        const rightPaneList = [];

        return (
            <React.Fragment>
                <table style={styles.browser}>
                    <tbody>
                        <tr>
                            <td style={styles.browserPane}>
                                <BrowserDatabasesPane app={app} />
                            </td>
                            <td style={styles.browserPane}>
                                <BrowserDocumentsPane app={app} />
                            </td>
                            <td style={styles.browserPane}>
                                <BrowserDocumentContentsPane />
                            </td>
                        </tr>
                    </tbody>
                </table>
            </React.Fragment>
        );
    }
}

// exports
// ---------------------------------------------------------

export default Browser;