import React, { Component } from 'react';
import './App.css';

import CancelLoginButton from './CancelLoginButton.js';
import LoginButton from './LoginButton.js';
import PasswordField from './PasswordField.js';
import UsernameField from './UsernameField.js';

// BrowserAuthPane styles
// ---------------------------------------------------------

const styles = {
    authErrorMessage: {
        marginBottom: '0rem',
        marginLeft: '1rem',
        marginTop: '0.5rem',
    },
    browserPane: {
        border: '1px solid black',
        height: '16rem',
        overflow: 'auto',
        padding: '6px',
        width: '24rem',
    },
    browserPaneTitle: {
        fontWeight: 'bold',
        height: '1rem',
    },
};

// BrowserAuthPane class
// ---------------------------------------------------------

class BrowserAuthPane extends Component {

    // main render
    // ---------------------------------------------------------

    render() {
        const pane = this;
        const app = pane.props.app;
        const dbName = app.getSelectedDatabase();
        const authErrorMessage = app.getAuthErrorMessage();

        return (
            <div>
                <div><p style={styles.browserPaneTitle}>Log in:</p></div>
                <div style={styles.browserPane}>
                    <p style={styles.authErrorMessage}>
                        {(authErrorMessage) ? authErrorMessage
                            : ("Database: '"+dbName+"'")}
                    </p>
                    <UsernameField />
                    <PasswordField />
                    <LoginButton
                        app={app}
                        database={dbName} />
                    <CancelLoginButton
                        app={app}
                        database={dbName} />
                </div>
            </div>
        );
    }
}

// exports
// ---------------------------------------------------------

export default BrowserAuthPane;

