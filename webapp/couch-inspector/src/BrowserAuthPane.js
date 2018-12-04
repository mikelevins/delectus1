import React, { Component } from 'react';
import './App.css';

import LoginButton from './LoginButton.js';
import TextField from '@material-ui/core/TextField';

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
    textField: {
        marginLeft: '1rem',
        marginTop: '0.5rem',
        width: '95%',
    },
};

// Auxiliary components
// ---------------------------------------------------------

function PasswordField(props) {
    return (
        <div>
            <TextField
                id="password"
                label='Password:'
                style={styles.textField}
                type='password'
                defaultValue=''
            />
        </div>
    );
}

function UsernameField(props) {
    return (
        <div>
            <TextField
                id="username"
                label='Username:'
                style={styles.textField}
                defaultValue=''
            />
        </div>
    );
}

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
                <div><p style={styles.browserPaneTitle}>Log in to '{dbName}'</p></div>
                <div style={styles.browserPane}>
                    <p style={styles.authErrorMessage}>
                        {(authErrorMessage) ? authErrorMessage
                            : 'Enter your username and password:'}
                    </p>
                    <UsernameField />
                    <PasswordField />
                    <LoginButton
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

