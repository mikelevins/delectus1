import React, { Component } from 'react';
import './App.css';

import Button from '@material-ui/core/Button';

// BrowserAuthPane styles
// ---------------------------------------------------------

const styles = {
    browserPane: {
        border: '1px solid black',
        height: '24rem',
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

// BrowserAuthPane class
// ---------------------------------------------------------

class BrowserAuthPane extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const app = pane.props.app;

        return (
            <div>
                <div>
                    <p style={styles.browserPaneTitle}>Log In</p>
                </div>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default BrowserAuthPane;

