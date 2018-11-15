import React, { Component } from 'react';
import './App.css';
import BrowserPane from './BrowserPane.js';

const styles = {
    browser: {
        border: '1px solid black',
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
        const keyPath = browser.props.app.keyPath;
        const activeKeyPath = (keyPath) ? (lastThree(keyPath)) : ([]);

        return (
            <React.Fragment>
                <table style={styles.browser}>
                    <tbody>
                        <tr>
                            <td><BrowserPane list={[]} /></td>
                            <td><BrowserPane list={[]} /></td>
                            <td><BrowserPane list={[]} /></td>
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