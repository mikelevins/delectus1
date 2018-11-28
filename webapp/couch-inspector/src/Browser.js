import React, { Component } from 'react';
import './App.css';
import BrowserDatabasesPane from './BrowserDatabasesPane.js';
import BrowserDocumentsPane from './BrowserDocumentsPane.js';
import BrowserDocumentContentsPane from './BrowserDocumentContentsPane.js';

// Browser styles
// ---------------------------------------------------------

const styles = {
    browser: {
        borderSpacing: '8px 8px',
    },
    browserPane: {
        padding: '10px 5px',
        verticalAlign: 'top',
    },
};

// Browser class
// ---------------------------------------------------------

class Browser extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const browser = this;
        const app = browser.props.app;

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
                                <BrowserDocumentContentsPane app={app} />
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