import React, { Component } from 'react';
import './App.css';
import BrowserPane from './BrowserPane.js';

const styles = {
    browser: {
        border: '1px solid black',
        borderSpacing: '8px 8px',
    },
    browserPane: {
        verticalAlign: 'top',
    },
};

function lastThree(anArray) {
    const len = anArray.length;

    if (len <= 3) {
        return anArray;
    } else {
        const start = len - 3;
        return anArray.slice(start, len);
    }
}

class Browser extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const browser = this;
        const app = browser.props.app;
        const keyPath = browser.props.app.keyPath;
        const activeKeyPath = (keyPath) ? (lastThree(keyPath)) : ([]);

        return (
            <React.Fragment>
                <table style={styles.browser}>
                    <tbody>
                        <tr>
                            <td style={styles.browserPane}>
                                <BrowserPane
                                    title='Databases'
                                    list={app.state.databases}
                                />
                            </td>
                            <td style={styles.browserPane}>
                                <BrowserPane
                                    title=''
                                    list={[]}
                                />
                            </td>
                            <td style={styles.browserPane}>
                                <BrowserPane
                                    title=''
                                    list={[]}
                                />
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